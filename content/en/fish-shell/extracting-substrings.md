---
title:                "Fish Shell recipe: Extracting substrings"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to extract a specific part of a string in your Fish Shell programming? Maybe you want to extract a username from a URL or a timestamp from a log file. Whatever the reason may be, learning how to extract substrings can save you time and make your code more efficient.

## How To

Coding in Fish Shell is all about simplicity and elegance. Using the `string` command, we can easily extract substrings from a given string. Let's take a look at some examples using the `string` command along with the `contains` and `match` functions.

```
# Extracting a substring between two known characters
set url "https://www.example.com/users/JohnDoe"
string split "/" $url
contains JohnDoe $string[4] # Output: 1 (true)
```

In the example above, we use the `string` command to split the `url` variable into an array, using the "/" character as the delimiter. Then, we use the `contains` function to check if the array contains "JohnDoe", which is the username we want to extract.

```
# Extracting a substring using a regular expression
set log "2021-01-10 12:34:56 | INFO | This is a log message"
match -r "([0-9]{2}):([0-9]{2}):([0-9]{2})" $log
echo $math[1] # Output: 12
echo $math[2] # Output: 34
echo $math[3] # Output: 56
```

In this example, we use the `match` function with a regular expression to extract the timestamp from the log message. The `match` function returns an array with the matched groups, which we can then access using indices.

## Deep Dive

The `contains` function and the `-r` flag for the `match` function are just two ways you can extract substrings in Fish Shell. There are also other useful string-related functions such as `index`, `startswith`, and `endswith` that can be used for substring extraction.

Additionally, you can also use the `sub` function to replace substrings in a given string. This can be useful if you only need to change a part of a string while keeping the rest intact.

## See Also

- [Fish Shell documentation on string functions](https://fishshell.com/docs/current/cmds/string.html)
- [Regular Expressions tutorial for Fish Shell](https://fishshell.com/docs/current/tutorial.html#tutorial-grep)
- [Fish Shell tutorials on YouTube](https://www.youtube.com/c/FishShellOfficial/playlists)