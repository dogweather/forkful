---
title:                "Working with yaml"
html_title:           "Fish Shell recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why

Working with YAML files is essential for any programmer, as it allows for easy configuration and data storage. YAML is a human-readable data format that is commonly used in various applications, making it a valuable skill to have as a developer.

## How To

To work with YAML files in Fish Shell, we need to use the `yq` tool. First, we need to install it using `brew`:

```
brew install yq
```

Next, we can use `yq` to read from and write to YAML files. Let's start by creating a simple YAML file called `config.yml` with the following content:

```
fruit:
  - apple
  - banana
  - orange
price: 2.50
```

To read the contents of this file, we can use the `yq read` command:

```
yq read config.yml
```

This will output the YAML data in a readable format. We can also use `yq` to modify the contents of the YAML file. For example, if we want to change the price of apples to $3.00, we can use the `yq write` command:

```
yq write config.yml fruit[0] "apple 3.00"
```

This will update our `config.yml` file with the new price. We can also use `yq` to format and validate YAML files, making it a powerful tool for working with data.

## Deep Dive

Behind the scenes, `yq` uses the `jq` library to manipulate YAML and JSON data. This means that we can also use `jq` commands to work with YAML files in Fish Shell. Additionally, Fish Shell has built-in support for working with YAML files, as it can handle indented lists and key-value pairs without the need for extra syntax.

One useful trick is to use the `grep` command to filter YAML data. For example, if we want to only see the fruit list in our `config.yml` file, we can use the following command:

```
grep fruit config.yml
```

This will output only the fruit list. We can also pipe this output to other commands, such as `sed` to modify the data, or `wc` to count the number of items in the list.

## See Also

- [Official Fish Shell documentation on YAML](https://fishshell.com/docs/current/tutorial.html#accessing-list-items)
- [YAML documentation](https://yaml.org/) 
- [`yq` repository on GitHub](https://github.com/kislyuk/yq)