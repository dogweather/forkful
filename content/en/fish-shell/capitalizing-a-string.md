---
title:                "Capitalizing a string"
html_title:           "Fish Shell recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Do you want to make your code a little more user-friendly? Or maybe you just want to add a touch of professionalism? Either way, capitalizing a string can make a big difference in how your output looks. Plus, with Fish Shell, it's quick and easy to do.

## How To

Capitalizing a string in Fish Shell is simple. Just follow these steps:

1. Open up your Fish Shell terminal.
2. Use the `string capitalize` command, followed by the string you want to capitalize, within Fish Shell's "```" code block.
3. Hit enter, and voila! Your string will now be capitalized.

Here's an example:

```Fish Shell
string capitalize "fish shell"
```

Output:
```
Fish Shell
```

## Deep Dive

While the `string capitalize` command is the easiest way to capitalize a string in Fish Shell, it's not the only way. You can also use the `tr` command to capitalize a string, although it requires a bit more code. Here's an example:

```Fish Shell
echo "fish shell" | tr '[a-z]' '[A-Z]'
```

Output:
```
FISH SHELL
```

You can also use the `sed` command to capitalize a string, but again, it requires more code. Here's an example:

```Fish Shell
echo "fish shell" | sed -e 's/^./\u&/' -e 's/.*/\L&/'
```

Output:
```
Fish Shell
```

Ultimately, the `string capitalize` command is the most efficient way to capitalize a string in Fish Shell.

## See Also

- [Fish Shell documentation](https://fishshell.com/docs/current/)
- [Learn Fish Shell in Y minutes](https://learnxinyminutes.com/docs/fish/)