---
title:                "Working with YAML"
date:                  2024-01-19
html_title:           "Arduino recipe: Working with YAML"
simple_title:         "Working with YAML"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Working with YAML means parsing and generating YAML (Yet Another Markup Language) documents in Python. Programmers do it to manage configuration files, application settings, or data serialization that's easy for humans to read and write.

## How to:
To work with YAML in Python, you need `pyyaml`. Install it using:

```Python
pip install pyyaml
```

Read a YAML file:

```Python
import yaml

with open('config.yaml', 'r') as stream:
    try:
        config = yaml.safe_load(stream)
        print(config)
    except yaml.YAMLError as exc:
        print(exc)
```

Write to a YAML file:

```Python
config = {'database': {'host': 'localhost', 'port': 3306}}

with open('config.yaml', 'w') as file:
    yaml.dump(config, file, default_flow_style=False)
```

Here's how `config.yaml` looks:

```yaml
database:
  host: localhost
  port: 3306
```

## Deep Dive
YAML launched in 2001 as a human-friendly data serialization standard for all programming languages. JSON and XML are alternatives, but YAML’s focus on readability is a standout feature. When parsing, `safe_load` is crucial to prevent arbitrary code execution due to unsafe YAML content. `default_flow_style=False` keeps the output non-JSON-like, preserving YAML’s readability.

## See Also
- Official PyYAML Documentation: https://pyyaml.org/wiki/PyYAMLDocumentation
- YAML Spec: https://yaml.org/spec/1.2/spec.html
- JSON vs. YAML comparison: https://csrc.nist.gov/csrc/media/projects/cryptographic-standards-and-guidelines/documents/examples/data-serialization.pdf
