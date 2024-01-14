---
title:                "Fish Shell recipe: Capitalizing a string"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

As a Fish Shell programmer, you may come across situations where you need to capitalize a string. For example, when dealing with user inputs or generating titles for your scripts. In these cases, it is important to know how to properly capitalize a string to ensure consistency and readability in your code.

## How To

Capitalizing a string in Fish Shell is a simple process that can be achieved in a few different ways, depending on your specific needs and preferences.

One method is by using the `string match` command. This will take a given string and convert the first character to uppercase. Here's an example:

```
string match --capitalize "hello world"
```

This will output "Hello world".

Another option is to use the `string toupper` command. This will convert all characters in a string to uppercase. For instance:

```
string toupper "fish shell"
```

The output will be "FISH SHELL".

You can also use the `tr` command to achieve the same result. Here's an example:

```
echo "fish shell" | tr "[:lower:]" "[:upper:]"
```

This will also result in "FISH SHELL".

## Deep Dive

While the above methods are suitable for most cases, it's important to note that they may not work as expected for strings that contain special characters or foreign characters. In these situations, it may be necessary to use regular expressions or more advanced methods to properly capitalize the string.

Additionally, it's important to consider the language and intended use of the string when deciding on the capitalization method. For example, in some languages, only the first letter of a sentence should be capitalized, while in others, proper nouns and acronyms may also be capitalized.

## See Also

- [Fish documentation on string manipulation](https://fishshell.com/docs/current/cmds/string.html)
- [Fish tutorial on regular expressions](https://fishshell.com/docs/current/tutorial.html#using-regular-expressions)
- [List of supported languages in Fish Shell](https://fishshell.com/docs/current/#supported-languages)