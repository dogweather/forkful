---
date: 2024-02-03 19:03:17.132416-07:00
description: "YAML, which stands for YAML Ain't Markup Language, is a human-readable\
  \ data serialization standard that can be used for configuration files, as well\
  \ as in\u2026"
lastmod: '2024-02-25T18:49:56.701407-07:00'
model: gpt-4-0125-preview
summary: "YAML, which stands for YAML Ain't Markup Language, is a human-readable data\
  \ serialization standard that can be used for configuration files, as well as in\u2026"
title: Working with YAML
---

{{< edit_this_page >}}

## What & Why?

YAML, which stands for YAML Ain't Markup Language, is a human-readable data serialization standard that can be used for configuration files, as well as in applications where data is being stored or transmitted. Programmers gravitate towards YAML due to its clarity and simplicity, especially in projects involving complex configurations or the need for easily editable data structures.

## How to:

Working directly with YAML in Bash requires a bit of ingenuity since Bash does not have built-in support for parsing YAML. However, you can use external tools like `yq` (a lightweight and portable command-line YAML processor) to interact with YAML files efficiently. Let's go through some common operations:

### Installing `yq`:

Before diving into the examples, ensure you have `yq` installed. You can usually install it from your package manager, for example, on Ubuntu:

```bash
sudo apt-get install yq
```

Or you can download it directly from its GitHub repository.

### Reading a value:

Consider you have a file named `config.yaml` with the following content:

```yaml
database:
  host: localhost
  port: 5432
user:
  name: admin
  password: secret
```

To read the database host, you can use `yq` as follows:

```bash
yq e '.database.host' config.yaml
```

**Sample Output:**

```
localhost
```

### Updating a value:

To update the user's name in `config.yaml`, use the `yq eval` command with the `-i` (in-place) option:

```bash
yq e '.user.name = "newadmin"' -i config.yaml
```

Verify the change with:

```bash
yq e '.user.name' config.yaml
```

**Sample Output:**

```
newadmin
```

### Adding a new element:

To add a new element under the database section, like a new field `timeout`:

```bash
yq e '.database.timeout = 30' -i config.yaml
```

Checking the contents of the file will confirm the addition.

### Deleting an element:

To remove the password under user:

```bash
yq e 'del(.user.password)' -i config.yaml
```

This operation will remove the password field from the configuration.

Remember, `yq` is a powerful tool and has a lot more capabilities, including converting YAML to JSON, merging files, and even more complex manipulations. Refer to the `yq` documentation for further exploration.
