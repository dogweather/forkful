---
title:                "Working with yaml"
html_title:           "Arduino recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML ain't Markup Language (YAML) is a human-readable data serialization standard. Programmers use it for config files, data storage, and inter-process messaging because of its simplicity and readability.

## How to:
Here's a simple example of reading a YAML file using Bash. 

Given `config.yaml`:
```yaml
database:
  host: localhost
  port: 3306
  username: user
  password: pass123
```

Use this script to read the YAML and print the database host:

```Bash
#!/bin/bash
value=$(grep 'host:' config.yaml | awk '{ print $2 }')
echo "Database Host: ${value}"
```

Sample output:
```
Database Host: localhost
```

## Deep Dive
YAML, created in 2001, is a more human-friendly alternative to XML or JSON. It's widely used in cloud services, app deployment, and devops tools. Though Bash lacks native YAML parsing, tools like `yq` and parsing via `awk` or `grep` can get the job done. However, complex parsing may need proper YAML tooling.

## See Also
- YAML official website: https://yaml.org
- `yq`, a command-line YAML processor: https://github.com/kislyuk/yq
- Bash YAML parsing discussion: https://stackoverflow.com/questions/5014632/how-can-i-parse-a-yaml-file-from-a-linux-shell-script