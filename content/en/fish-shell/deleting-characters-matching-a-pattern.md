---
title:                "Deleting characters matching a pattern"
html_title:           "Fish Shell recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why
If you're an avid user of Fish Shell, you may encounter situations where you need to delete certain characters that match a specific pattern. Whether it's to clean up messy data or make your code more efficient, mastering this skill can be incredibly useful in your daily programming tasks.

## How To
To delete characters matching a pattern in Fish Shell, you can use the `string replace` command. Let's say you have a string `hello123world` and you want to remove all numbers from it. The command would look like this:

```Fish Shell
string replace hello123world \d ''  
```

The `\d` here represents the regex pattern for numbers, and the empty string `''` signifies that we want to replace it with nothing. The `replace` command will then output `helloworld` as the result.

## Deep Dive
For a more in-depth explanation, the `replace` command follows the syntax of `string replace INPUT REGEX SUBSTITUTION`, where INPUT is the original string, REGEX is the pattern you want to match, and SUBSTITUTION is what you want to replace it with. You can also use variations of the command, such as `string replace --count NUM`, which will only replace the first NUM occurrences, or `string replace --all`, which will replace all occurrences. Additionally, you can use the `replace-string` function to save the result to a variable for later use.

## See Also
For more information on using Fish Shell, check out these helpful resources:

- [Official Fish Shell documentation](https://fishshell.com/docs/current/index.html)
- [Learn X in Y minutes - Fish Shell](https://learnxinyminutes.com/docs/fish/)