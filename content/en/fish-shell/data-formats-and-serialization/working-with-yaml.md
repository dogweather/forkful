---
title:                "Working with YAML"
date:                  2024-02-03T19:03:10.609396-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Working with YAML involves parsing and manipulating YAML (YAML Ain't Markup Language) files, a data serialization format used for configuration files, in Fish Shell. Programmers do this to automate and configure applications or services efficiently within the context of shell environments, facilitating tasks like configurations management and resources provisioning.

## How to:
Fish Shell doesn’t have built-in support for parsing YAML, but you can utilize third-party tools like `yq` (a lightweight and portable command-line YAML processor) to handle YAML data.

**Installation of yq (if not already installed):**
```fish
sudo apt-get install yq
```

**Reading a value from a YAML file:**
Suppose you have a YAML file `config.yaml` with the following content:
```yaml
database:
  host: localhost
  port: 3306
```

To read the database host, you’d use:
```fish
set host (yq e '.database.host' config.yaml)
echo $host
```
**Sample output:**
```
localhost
```

**Updating a value in a YAML file:**
To update the `port` to `5432`, use:
```fish
yq e '.database.port = 5432' -i config.yaml
```
**Verify the update:**
```fish
yq e '.database.port' config.yaml
```
**Sample output:**
```
5432
```

**Writing a new YAML file:**
For creating a new `new_config.yaml` with predefined content:
```fish
echo "webserver:
  host: '127.0.0.1'
  port: 8080" | yq e -P - > new_config.yaml
```
This uses `yq` to process and pretty-print (-P flag) a string into a new YAML file.

**Parsing complex structures:**
If you have a more complex YAML file and need to fetch nested arrays or objects, you can:
```fish
echo "servers:
  - name: server1
    ip: 192.168.1.101
  - name: server2
    ip: 192.168.1.102" > servers.yaml

yq e '.servers[].name' servers.yaml
```
**Sample output:**
```
server1
server2
```
Using `yq`, Fish Shell makes it straightforward to navigate through YAML documents and manipulate them for various automation and configuration tasks.
