---
title:                "Searching and replacing text"
html_title:           "Fish Shell recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Searching and replacing text is a common task in programming, especially when working with large files or repetitive tasks. Using the Fish Shell's built-in functions for searching and replacing offers a more efficient and streamlined approach to this task, saving you time and effort.

## How To

To search and replace text in the Fish Shell, we will be using the `sed` and `awk` commands. These commands allow us to specify patterns in a text file and replace them with new text.

To search and replace using `sed`, we use the following syntax:

```
Fish Shell:
sed 's/search/replace/g' file.txt
```

This command will search for the pattern "search" in the file "file.txt" and replace it with "replace". The "g" at the end of the command ensures that all instances of the pattern are replaced, not just the first one.

For example, if we have a file named "test.txt" with the following content:

```
This is a test sentence.
```

And we want to replace all instances of the word "test" with "replace", we can use the following command:

```
Fish Shell:
sed 's/test/replace/g' test.txt
```

The output will be:

```
This is a replace sentence.
```

`awk` works in a similar way, but allows us to specify more complex patterns. Here's the syntax for using `awk`:

```
Fish Shell:
awk '{gsub(/pattern1/, "replacement string"); print}' file.txt
```

Using the same example as before, we can use `awk` to replace all instances of "test" with "replace" with the following command:

```
Fish Shell:
awk '{gsub(/test/, "replace"); print}' test.txt
```

The output will be the same as the `sed` command.

## Deep Dive

Both `sed` and `awk` have more advanced options for searching and replacing text. For example, we can use regular expressions with `sed`, allowing us to specify more complex patterns to search for. We can also use flags such as "i" to make the search case-insensitive.

Similarly, `awk` allows us to specify more complex patterns using regular expressions. It also has additional built-in functions for manipulating text, making it a powerful tool for searching and replacing.

For more information on the syntax and available options for `sed` and `awk`, refer to their respective documentations.

## See Also

- [Fish Shell Official Documentation](https://fishshell.com/docs/current/)
- [sed Command Documentation](https://www.gnu.org/software/sed/manual/sed.html)
- [awk Command Documentation](https://www.gnu.org/software/gawk/manual/gawk.html)