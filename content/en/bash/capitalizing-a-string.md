---
title:                "Capitalizing a string"
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string means changing the first letter of each word to uppercase. Programmers do this for formatting, consistency, and readability, especially in titles or headings.

## How to:

In Bash, you can capitalize strings in several ways. Here's a classic approach using `awk`:

```Bash
echo "hello world" | awk '{for(i=1;i<=NF;i++) $i=toupper(substr($i,1,1)) substr($i,2)} 1'
```

Output:
```
Hello World
```

Or, with pure Bash:

```Bash
string="hello world"
capitalize() {
  echo "$1" | while IFS=" " read -r word; do 
    echo -n "${word^} " 
  done
  echo
}
capitalize "$string"
```

Output:
```
Hello World 
```

## Deep Dive:

Back in the day, ‘awk’ was the go-to tool for text manipulation. It’s robust but less intuitive for beginners. With the evolution of Bash, especially from version 4 onwards, capabilities like string manipulation have improved.

The `awk` way is classic, iterating through each word and capitalizing the first letter. Pure Bash uses parameter expansion: `${word^}` capitalizes the first letter of `$word`. Parameter expansion is direct and quick, cutting down on the number of external tools needed.

Why does this matter? Well, capitalizing strings is a common need across programming tasks. Proper capitalization can be critical for user interfaces or data processing where presentation matters. Knowing how to do this in your shell can save the day.

## See Also:

- Bash manual for parameter expansion: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion
- `awk` introduction and usage: https://www.gnu.org/software/gawk/manual/gawk.html
- StackOverflow discussions on text manipulation in Bash: https://stackoverflow.com/questions/tagged/bash+string+capitalization