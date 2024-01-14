---
title:    "Fish Shell recipe: Converting a string to lower case"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting strings to lower case is a common task in programming, especially when manipulating user-inputted data. It ensures consistency and makes it easier to compare strings for equality.

## How To

To convert a string to lower case in Fish Shell, we can use the `string tolower` command. It takes in a string as an argument and returns the same string in lower case.

```
Fish Shell ~> string tolower "HeLLo"
hello
```

We can also use the `tr` command to achieve the same result. The `tr` (translate) command can perform character substitutions, and in this case, we can use it to change all uppercase letters to lowercase.

```
Fish Shell ~> echo "HeLLo" | tr '[:upper:]' '[:lower:]'
hello
```

We can also use the `awk` command to convert the string to lower case. This method might seem a bit unconventional, but it can be handy if we want to manipulate the string further in our script.

```
Fish Shell ~> echo "HeLLo" | awk '{print tolower($0)}'
hello
```

We can also wrap these commands in a function for reusability. Here's an example of how we can create a `tolower` function in Fish Shell:

```
Fish Shell ~> function tolower
      for str in $argv
          string tolower $str | sed "s/.*/&/"
      end
end
```

Now we can use this function to convert multiple strings to lower case:

```
Fish Shell ~> tolower "HeLLo" "WoRLd"
hello world
```

## Deep Dive

Converting strings to lower case might seem like a trivial task, but it can get more complex when dealing with non-English characters or special symbols. In such cases, using the `tr` command might not give accurate results.

In Fish Shell, we can use the `string--lowercase` function from the `string` module to handle special characters and non-English characters. This function takes in a string as an argument and returns the same string in lower case, taking into consideration the locale settings.

```
Fish Shell ~> source /usr/share/fish/scripts/string.fish
Fish Shell ~> set mystr "hEllo👋"
Fish Shell ~> string--lowercase $mystr
hello👋
```

## See Also

- [Fish Shell string tolower documentation](https://fishshell.com/docs/current/cmds/string-tolower.html)
- [Fish Shell tr documentation](https://fishshell.com/docs/current/cmds/tr.html)
- [Fish Shell awk documentation](https://fishshell.com/docs/current/cmds/awk.html)
- [Fish Shell string module documentation](https://fishshell.com/docs/current/index.html#string)