---
title:    "Fish Shell recipe: Deleting characters matching a pattern"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Deleting characters matching a pattern is a common task in programming. This simple yet powerful action can help you clean up data, manipulate text, or remove unwanted characters from a string. With the help of Fish Shell, this process can be completed efficiently and effectively.

## How To

To delete characters matching a pattern in Fish Shell, we will use the `sed` command. This command allows us to perform text transformations based on a specified pattern. Here is an example of how to use `sed` to delete all occurrences of the letter "a" in a string:

```
Fish Shell> echo "apple, banana, carrot" | sed 's/a//g'
pple, bn, crt
```

In this command, we use the `echo` command to output a string, then pipe it (`|`) to the `sed` command. Inside the `sed` command, we specify the pattern we want to delete (`a`) and the replacement (`/g`). This tells `sed` to find all occurrences of "a" and replace them with nothing, effectively deleting them.

We can also use regular expressions to specify a more complex pattern to delete. For example, we can delete all vowels in a string using the pattern `[aeiou]`:

```
Fish Shell> echo "hello world" | sed 's/[aeiou]//g'
hll wrld
```

This command will delete all vowels from the string "hello world" and output "hll wrld".

## Deep Dive

In the previous examples, we used the global (`g`) flag to delete all occurrences of the pattern in a string. However, we can also specify a specific occurrence or range using the `sed` command's address range feature.

For example, `sed '1,3s/[aeiou]//g'` will only delete vowels in the first three lines of a file, while `sed '2,4s/a/1/g'` will replace all "a" in lines 2, 3, and 4 with "1". This allows for more precise control over which characters are deleted and where.

In addition to the `sed` command, Fish Shell also has a built-in `string` command that allows for more complex string manipulation. We can use the `string` command in combination with `sed` to perform even more powerful operations on our strings, such as deleting a specific word or character at a specific position.

## See Also

- Fish Shell documentation on `sed`: https://fishshell.com/docs/current/cmds/sed.html
- Fish Shell documentation on `string`: https://fishshell.com/docs/current/cmds/string.html