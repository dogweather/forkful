---
title:    "Fish Shell recipe: Extracting substrings"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why
Substrings are a useful concept in programming, as they allow us to extract specific parts of a larger string. In the Fish Shell, extracting substrings can be particularly helpful when dealing with file names or system outputs. It allows us to easily manipulate and use specific sections of text without having to manually search for them.

## How To
To extract substrings in Fish Shell, we can use the `string` command followed by the `-r` flag to specify the substring we want to extract. For example, if we have a string called `filename` with the value of `my_file.txt`, we can use the following code to extract just the file extension:

```Fish Shell
string -r ".txt" $filename
```

This would return the value `txt`. We can also specify a range of characters to extract by using the `-s` flag for the starting index and the `-l` flag for the length. For example, if we wanted to extract the first 5 characters of a string, we could use the following code:

```Fish Shell
string -s 1 -l 5 "Hello world"
```

This would return the value `Hello`.

## Deep Dive
The `string` command is actually a shortcut for the `string match` command, which allows for more advanced substring extraction. We can use regular expressions to define the pattern we want to match and extract. For example, if we have a string called `phone_number` with the value of `123-456-7890`, we can use the following code to extract just the area code:

```Fish Shell
string match -r "([0-9]{3})-[0-9]{3}-[0-9]{4}" $phone_number
```

This would return the value `123`, as it matches the first group of 3 numbers within the parentheses.

## See Also
- Official Fish Shell documentation on string manipulation: https://fishshell.com/docs/current/cmds/string.html
- A guide to regular expressions in Fish Shell: https://fishshell.com/docs/current/tutorial.html#tut_regex