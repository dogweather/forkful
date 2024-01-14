---
title:                "Bash recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Have you ever come across a situation where you need to delete a specific set of characters from a file or text? Maybe they are typos or unwanted symbols that you want to remove. In such cases, knowing how to delete characters matching a pattern in Bash can come in handy.

## How To

To delete characters matching a pattern in Bash, you need to use the `tr` command. This command allows you to translate or delete characters in a file or text. Here's an example of how you can use `tr` to delete the letter "a" from a file:

```Bash
echo "apple" | tr -d 'a'
```

The above command will output "pple" after deleting the letter "a".

You can also provide a set of characters to be deleted instead of just one. For example, if you want to delete vowels from a file, you can use the following command:

```Bash
echo "hello" | tr -d 'aeiou'
```

This will output "hll" after deleting all the vowels.

You can also specify a range of characters to be deleted using the `-c` option. For example, if you want to delete all numbers from a file, you can use the following command:

```Bash
echo "abc123" | tr -d -c 'a-z'
```

This will output "abc" after deleting all the numbers.

## Deep Dive

The `tr` command is useful for deleting characters because it processes the input one character at a time and replaces it with the specified character. This makes it efficient for large files or texts.

Another advantage of using `tr` is that it allows you to delete characters from stdin as well as from a file. This means you can use this command in your scripts to automate the deletion of characters.

It's also worth mentioning that you can use regular expressions with `tr` to delete patterns of characters instead of just single characters. Regular expressions provide much more flexibility in terms of what you can delete, making the `tr` command even more powerful.

## See Also

To learn more about the `tr` command and its options, you can check out the following resources:

- [Bash tr command](https://www.gnu.org/savannah-checkouts/gnu/coreutils/manual/html_node/tr-invocation.html)
- [Linux tr command](https://linux.die.net/man/1/tr)
- [Bash regex tutorial](https://linuxconfig.org/bash-regex)
- [Bash scripting guide](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)

Happy coding!