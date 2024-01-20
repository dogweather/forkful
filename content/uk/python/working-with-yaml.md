---
title:                "Робота з YAML"
html_title:           "Arduino: Робота з YAML"
simple_title:         "Робота з YAML"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
## Що і Чому?

YAML stands for "YAML Ain't Markup Language". It's a human-readable data serialization format. Programmers use it for config files, data storage, or sharing data between different languages because it's simple and readable.

## How to:
## Як це зробити:

To work with YAML in Python, you'll need the `pyyaml` library. Install it using `pip`:

```bash
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
import yaml

data = {'database': {'user': 'root', 'password': 's3cr3t'}}

with open('config.yaml', 'w') as outfile:
    yaml.dump(data, outfile, default_flow_style=False)
```

Sample `config.yaml` contents:

```yaml
database:
  user: root
  password: s3cr3t
```

## Deep Dive:
## Поглиблене вивчення:

YAML came to life in the early 2000s as a human-friendly data format alternative to XML or JSON. While JSON is often used in web services, YAML is preferred for config files due to its clear syntax. In Python, YAML is typically parsed with `pyyaml`, but alternatives like `ruamel.yaml` offer extended functionality like preserving comments.

## See Also:
## Дивись також:

- Official YAML website for spec details: [YAML](https://yaml.org)
- PyYAML Documentation: [PyYAML](https://pyyaml.org/wiki/PyYAMLDocumentation)
- Alternative Python library `ruamel.yaml`: [ruamel.yaml](https://sourceforge.net/projects/ruamel-yaml/)
- Comparison of data serialization formats: [Data Formats](https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats)