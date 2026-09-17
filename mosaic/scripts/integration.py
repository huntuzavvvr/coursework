"""Black-box CLI tests. Uses only Python's standard library and the built CLI."""
import json
import re
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
CLI = [str(ROOT / "scripts/dotnet.sh"), str(ROOT / "src/Mosaic.Cli/bin/Release/net10.0/Mosaic.Cli.dll")]


def invoke(*args, code=0):
    result = subprocess.run(CLI + list(args), cwd=ROOT, text=True, capture_output=True, timeout=30)
    assert result.returncode == code, (args, result.returncode, result.stdout, result.stderr)
    return result


def outcomes(report):
    return {item["value"]: item["probability"] for item in report["outcomes"]}


def example(name, expected, evidence="1"):
    report = json.loads(invoke("run", f"examples/{name}.mos", "--json").stdout)
    assert outcomes(report) == expected, (name, report)
    assert report["evidence"] == evidence, (name, report)


def main():
    example("factorial", {"2432902008176640000": "1"})
    example("shared-weather", {'["sun" "sun"]': "3/4", '["rain" "rain"]': "1/4"})
    example("lazy-choice", {'["sun" "sun"]': "3/4", '["rain" "rain"]': "1/4"})
    example("dice", {f"[{n} {7-n}]": "1/6" for n in range(1, 7)}, "1/6")
    example("detective", {"false": "1/2", "true": "1/2"}, "9/50")
    example("expedition", {'["ridge" 4]': "3/7", '["valley" 6]': "3/7", '["valley" 8]': "1/7"}, "7/8")
    example("streams", {"[0 1 1 2 3 5 8 13 21 34 55 89]": "1"})
    example("closures", {"[32 50 68 212]": "1"})
    example("lists", {"[[4 16 36 64 100] 220 [9 4 1]]": "1"})
    example("queens", {"[1 3 0 2]": "1/2", "[2 0 3 1]": "1/2"}, "1/128")
    example("tail-recursion", {"200010000": "1"})

    with tempfile.TemporaryDirectory(prefix="mosaic-test-") as directory:
        target = Path(directory) / "report.txt"
        invoke("run", "examples/files.mos", "--input", "message", "examples/message.txt", "--output", "report", str(target))
        assert target.read_text() == (ROOT / "examples/message.txt").read_text() + "\nProcessed by Mosaic.\n"
        target.write_text("original")
        ambiguous = '(write-text "report" (choose "coin" [[1 "a"] [1 "b"]]))'
        assert "E_EXPORT" in invoke("eval", ambiguous, "--output", "report", str(target), code=1).stderr
        assert target.read_text() == "original"
        missing = '(if (choose "coin" [[1 true] [1 false]]) (write-text "report" "a") "b")'
        assert "E_EXPORT" in invoke("eval", missing, "--output", "report", str(target), code=1).stderr
        assert target.read_text() == "original"
        assert "E_USAGE" in invoke("eval", "1", "--output", "a", str(target), "--output", "b", str(target), code=2).stderr
        invalid = Path(directory) / "invalid.mos"
        invalid.write_text("(let [x 1]\n  (+ x missing))")
        invoke("check", str(invalid))  # check intentionally does syntax checking only
        assert ":2:8: E_NAME" in invoke("run", str(invalid), code=1).stderr
        invoke("run", str(Path(directory) / "missing.mos"), code=1)

    assert "syntax OK" in invoke("check", "examples/dice.mos").stdout
    assert "posterior=" in invoke("run", "examples/detective.mos", "--explain").stdout
    assert "E_ZERO" in invoke("eval", "(/ 1 0)", code=1).stderr
    assert "E_STEPS" in invoke("eval", "(letrec [go (fn [n] (go n))] (go 0))", "--max-steps", "100", code=1).stderr
    assert "E_WORLDS" in invoke("run", "examples/queens.mos", "--max-worlds", "10", code=1).stderr
    invoke("eval", "1", "--max-steps", "0", code=2)
    invoke("eval", "1", "--unknown", code=2)
    invoke("eval", "1", "--input", "a", code=2)
    invoke("eval", "1", "--input", "a", "x", "--input", "a", "y", code=2)
    invoke("--help")
    assert "0.1.0" in invoke("--version").stdout
    repl = subprocess.run(CLI + ["repl"], input="(+ 20 22)\n:quit\n", cwd=ROOT, text=True, capture_output=True, timeout=30)
    assert repl.returncode == 0 and "42 @ 1" in repl.stdout

    # Structural guard against accidentally moving mutation or I/O into the pure core.
    forbidden = re.compile(r"\bmutable\b|\bref\s|<-|\b(?:File|Console|Random|Dictionary|ResizeArray)\.")
    for path in (ROOT / "src/Mosaic.Core").glob("*.fs"):
        assert not forbidden.search(path.read_text()), f"Pure-core audit failed: {path.name}"
    print("PASS: 12 examples, CLI diagnostics, JSON, REPL, file effects, limits, pure-core audit")


if __name__ == "__main__":
    main()
