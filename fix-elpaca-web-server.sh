#!/usr/bin/env bash
# Fix elpaca naming collision between skeeto/emacs-web-server (simple-httpd)
# and eschulte/emacs-web-server (web-server).
#
# Elpaca assigns the bare "emacs-web-server/" directory to whichever of the
# two packages is processed first, with the second getting the qualified
# ".github.<user>" suffix. If the on-disk state no longer matches the
# runtime-computed paths, activation fails. This script corrects the state.

ELPACA_DIR="$(dirname "$0")/elpaca"
SOURCES="$ELPACA_DIR/sources"
BUILDS="$ELPACA_DIR/builds"
BARE="$SOURCES/emacs-web-server"

set -e

if [ ! -d "$BARE" ]; then
    echo "No bare 'emacs-web-server/' directory found — nothing to fix."
    exit 0
fi

# Detect which repo is in the bare directory by checking for the main file.
if [ -f "$BARE/web-server.el" ]; then
    TARGET="$SOURCES/emacs-web-server.github.eschulte"
    echo "Bare directory contains eschulte's repo (web-server.el)."
elif [ -f "$BARE/simple-httpd.el" ]; then
    TARGET="$SOURCES/emacs-web-server.github.skeeto"
    echo "Bare directory contains skeeto's repo (simple-httpd.el)."
else
    echo "ERROR: Cannot identify repo in '$BARE' — no web-server.el or simple-httpd.el found."
    exit 1
fi

if [ -d "$TARGET" ]; then
    echo "ERROR: Target '$TARGET' already exists. Manual inspection needed."
    exit 1
fi

echo "Renaming '$BARE' -> '$TARGET'"
mv "$BARE" "$TARGET"

for pkg in simple-httpd web-server; do
    build="$BUILDS/$pkg"
    if [ -d "$build" ]; then
        echo "Deleting stale build: $build"
        rm -rf "$build"
    fi
done

echo "Done. Start Emacs — elpaca will rebuild simple-httpd and web-server from the correct sources."
