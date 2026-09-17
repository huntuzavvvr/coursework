#!/bin/sh
set -eu
mosaic_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
export DOTNET_CLI_TELEMETRY_OPTOUT=1
export DOTNET_SKIP_FIRST_TIME_EXPERIENCE=1
export DOTNET_NOLOGO=1
export DOTNET_CLI_HOME="${DOTNET_CLI_HOME:-$mosaic_root/.dotnet-home}"
if [ -n "${DOTNET:-}" ]; then
    exec "$DOTNET" "$@"
elif command -v dotnet >/dev/null 2>&1; then
    exec dotnet "$@"
elif [ -x "$mosaic_root/.tools/dotnet/dotnet" ]; then
    exec "$mosaic_root/.tools/dotnet/dotnet" "$@"
else
    printf '%s\n' 'Install .NET SDK 10: https://dotnet.microsoft.com/download/dotnet/10.0' >&2
    exit 127
fi
