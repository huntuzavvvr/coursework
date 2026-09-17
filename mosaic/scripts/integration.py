"""Check the example files and the command-line runner."""
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


def example(name, expected):
    actual = invoke(f"examples/{name}.mos").stdout.strip()
    assert actual == expected, (name, actual)


def main():
    example("factorial", "2432902008176640000")
    example("closures", "[3 5 13]")
    example("lists", "[[4 16 36 64 100] 220 [9 4 1]]")
    example("streams", "[0 1 1 2 3 5 8 13 21 34 55 89]")
    example("pipeline", "220")
    example("grades", "[[5 4 5 5] 4]")
    with tempfile.TemporaryDirectory(prefix="mosaic-test-") as directory:
        target = Path(directory) / "report.txt"
        program = Path(directory) / "test.mos"
        invoke("examples/files.mos", "--input", "message", "examples/message.txt", "--output", "report", str(target))
        assert target.read_text() == (ROOT / "examples/message.txt").read_text() + "\nProcessed by Mosaic.\n"
        target.write_text("original")
        for source, diagnostic in [
            ('[(write-text "report" "a") (write-text "report" "b")]', "E_OUTPUT"),
            ('42', "E_EXPORT"),
            ('(let [saved (write-text "report" "a")] (/ 1 0))', "E_ZERO"),
        ]:
            program.write_text(source)
            assert diagnostic in invoke(str(program), "--output", "report", str(target), code=1).stderr
            assert target.read_text() == "original"
        program.write_text('(write-text "report" "new")')
        assert "E_EXPORT" in invoke(str(program), "--output", "report", str(target),
                                    "--output", "missing", str(Path(directory) / "other.txt"), code=1).stderr
        assert target.read_text() == "original"
        program.write_text("(let [x 1]\n  (+ x missing))")
        assert ":2:8: E_NAME" in invoke(str(program), code=1).stderr
        program.write_text("(letrec [go (fn [n] (go n))] (go 0))")
        assert "E_RECURSION" in invoke(str(program), code=1).stderr
        invoke(str(Path(directory) / "missing.mos"), code=1)
        invoke("examples/factorial.mos", "--output", "a", str(target), "--output", "b", str(target), code=2)
    invoke("examples/factorial.mos", "--unknown", code=2)
    invoke("examples/factorial.mos", "--input", "a", code=2)
    invoke("examples/factorial.mos", "--input", "a", "x", "--input", "a", "y", code=2)
    invoke("--help")
    assert invoke("run", "examples/factorial.mos").stdout == invoke("examples/factorial.mos").stdout
    forbidden = re.compile(r"\bmutable\b|\bref\s|<-|\b(?:File|Console|Random|Dictionary|ResizeArray)\.")
    for path in (ROOT / "src/Mosaic.Core").glob("*.fs"):
        assert not forbidden.search(path.read_text()), f"Unexpected mutation or I/O: {path.name}"
    print("PASS: 7 examples, CLI errors, file I/O, recursion limit, pure core")


if __name__ == "__main__":
    main()
