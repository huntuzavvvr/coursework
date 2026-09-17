#!/bin/sh
set -eu
mosaic_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$mosaic_root"
./scripts/dotnet.sh build Mosaic.slnx --configuration Release --nologo -m:1 -nodeReuse:false
./scripts/dotnet.sh tests/Mosaic.Tests/bin/Release/net10.0/Mosaic.Tests.dll
python3 scripts/integration.py
