---
date: 2024-01-25 03:39:31.938698-07:00
description: "TOML is a config file format, easy for humans to read and write, and\
  \ easy for machines to parse and generate. Programmers work with TOML for clear,\u2026"
lastmod: '2024-03-13T22:45:00.494927-06:00'
model: gpt-4-1106-preview
summary: TOML is a config file format, easy for humans to read and write, and easy
  for machines to parse and generate.
title: Working with TOML
weight: 39
---

## How to:
To read and manipulate TOML in Fish, you might use a tool like `yj`, which can convert TOML to JSON. Here’s how:

```fish
# Install yj via Fisher
fisher install jorgebucaran/yj

# Convert TOML to JSON
echo 'title = "TOML Example"' | yj -tj

# Sample output
{"title":"TOML Example"}
```

To write TOML, you reverse the process:

```fish
# Convert JSON to TOML
echo '{"title":"JSON Example"}' | yj -jt

# Sample output
title = "JSON Example"
```

For heavy lifting, consider a dedicated TOML CLI tool like `toml-cli`.

```fish
# Install toml-cli
pip install toml-cli

# Set a value in TOML file
toml set pyproject.toml tool.poetry.version "1.1.4"

# Get a value from TOML file
set version (toml get pyproject.toml tool.poetry.version)
echo $version
```

## Deep Dive
TOML (Tom's Obvious, Minimal Language), introduced by Tom Preston-Werner in 2013, is akin to INI but with a defined spec and data hierarchy. JSON and YAML are the main alternatives, but they have their trade-offs: JSON isn't as human-friendly, while YAML is more complex. TOML's design thrives in scenarios where config files are often maintained by hand, balancing simplicity and expressiveness. When it comes to implementation, TOML parsers are available for most programming languages, including TomlBombadil for Fish that can slot right into your scripts.

## See Also
- TOML Official spec: https://toml.io
- `yj`, a tool to convert between TOML, JSON, YAML, and XML: https://github.com/jorgebucaran/yj
- `toml-cli`, a command-line utility for TOML: https://github.com/sdispater/toml-cli
