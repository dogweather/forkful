---
title:                "Extracting substrings"
html_title:           "Fish Shell recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

As a Fish Shell user, you may sometimes need to extract substrings from a string variable. This can be useful when manipulating file paths, parsing data, or performing other operations on strings. In this article, we will explore how to extract substrings using Fish Shell and learn about its various options and abilities.

## How To

To extract a substring from a string variable, we can use the `string sub` command in Fish Shell. The basic syntax for this command is:

```Fish Shell
string sub STRING VARIABLE START INDEX LENGTH
```

Let's look at an example. Suppose we have a string variable named `filename` which contains the value "my_file.docx". We want to extract the substring "file" from this variable. We can do so by using the following command:

```Fish Shell
string sub $filename 3 4
```

This will output "file" as the result. The first argument after the variable name (`3` in this case) represents the starting index of the substring and the second argument (`4`) represents the length of the substring we want to extract.

We can also use negative indices to indicate the start index from the end of the string. For example, `string sub $filename -4 4` would also output "file" as the result. 

Additionally, we can use the `string length` command to find the length of the string before extracting a substring. This can be useful when we don't know the exact length of the substring we want to extract. For example:

```Fish Shell
set start_index (string length $filename - 11)
string sub $filename $start_index 4
```

This would output "file" as the result, regardless of the length of the string before the substring we want to extract. 

To extract a substring using a specific character as a reference point, we can use the `string find` command. This command will return the index of the first occurrence of a character within the string. We can then use this index as the start index for the `string sub` command. For example:

```Fish Shell
set start_index (string find $filename "_")
string sub $filename $start_index 4
```

This would return "file" as the result, as we are starting at the index of the underscore character and extracting a substring of length 4. 

## Deep Dive

In addition to the basic functionality of extracting substrings, Fish Shell also offers various options to make this process more flexible and powerful.

One such option is the `-r` flag, which allows us to extract substrings in reverse. This can be useful when dealing with strings that have a known pattern at the end. For example:

```Fish Shell
string sub -r $filename 5 3
```

This would return "xcod" as the result, as we are starting 5 characters from the end of the string and extracting a substring of length 3.

We can also use the `string sub -p` command to extract a substring as well as print the remaining part of the string after the extraction. This can be useful when we want to apply the same operation to multiple parts of a string. For example:

```Fish Shell
string sub -p $filename 3 4
```

This would return "file" as the result and also print "my_.docx", as this is the remaining part of the string after the substring extraction.

Lastly, we can use the `string sub -q` command to suppress any output except for the extracted substring. This can be useful when we only want the result of the extraction and not the rest of the string. For example:

```Fish Shell
string sub -q $filename 3 4
```

This would simply return "file" as the result, without printing the rest of the string.

## See Also

For more information and examples on using the `string sub` command in Fish Shell, refer to the official documentation:

- [Fish Shell Documentation - string sub](https://fishshell.com/docs/current/cmds/string.html#string_sub)
- [Fish Shell Tutorial - Strings and Substrings](https://fishshell.com/docs/current/tutorial.html#tut_strings)
- [Fish Shell Cookbook - Working with Strings](https://fishshell.com/docs/current/cookbook.html#tut_strings)