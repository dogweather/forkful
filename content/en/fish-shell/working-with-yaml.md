---
title:                "Working with YAML"
date:                  2024-01-19
html_title:           "Arduino recipe: Working with YAML"
simple_title:         "Working with YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

YAML, "YAML Ain't Markup Language", is a human-friendly data serialization standard that's notationally superior to tabular and markup languages for config files and data exchange. Programmers use it for its simplicity and readability in configuration files, deployment manifests, and more complex data structures.

## How to:

### Reading YAML Config
```Fish Shell
# Assuming 'config.yaml' contains:
# name: Fishy
# occupation: Shell

set config (yaml2json < config.yaml | jq -r '.name, .occupation')
echo $config
# Output: Fishy Shell
```

### Writing to YAML File
```Fish Shell
# Using 'yq', a portable command-line YAML processor
echo -e "name: Nemo\ncolor: Orange" > fish.yaml

# Add a new key
yq e '.friends += ["Dory"]' -i fish.yaml

cat fish.yaml
# Output:
# name: Nemo
# color: Orange
# friends:
# - Dory
```

## Deep Dive

YAML emerged in the early 2000s as a simplification of XML and has since become a standard for configuration files in the software industry. Its minimal syntax is both a boon and a bane—easy to read but tricky to parse without libraries. Alternatives to YAML include JSON, XML, and TOML, each with its own use case trade-offs. In Fish Shell, `yq` and `yaml2json` are commonly used for manipulating YAML files since Fish lacks built-in YAML parsing.

## See Also

- YAML official site: https://yaml.org
- `jq` manual: https://stedolan.github.io/jq/manual/
- `yq` repository and documentation: https://github.com/mikefarah/yq
