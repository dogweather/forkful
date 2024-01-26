---
title:                "Zamiana liter na wielkie w ciągu znaków"
html_title:           "Arduino: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Capitalizing a string means changing the first letter of each word to uppercase. We do this to follow grammatical rules, improve readability, or meet formatting requirements in programming outputs.

## How to (Jak to zrobić):
Fish Shell doesn't have a native function to capitalize strings directly, but with a little creativity, you can use string manipulation commands to achieve it. Check this out:

```Fish Shell
function capitalize
    set -l words (string split " " $argv)
    for word in $words
        set -l first_char (string sub -l 1 -- $word)
        set -l rest (string sub -s 2 -- $word)
        echo -n (string to-upper $first_char)(string to-lower $rest)" "
    end
end

set sample_text "fish shell programming is quite fun"
capitalize $sample_text
```

Output:
```
Fish Shell Programming Is Quite Fun 
```

## Deep Dive (Dogłębna analiza):
Capitalizing strings isn't a new concept; it's been around since early text processors. In some shells like Bash, you might use `awk` or a `for` loop with string manipulation too. Fish takes a modern approach, leveraging its own string manipulation functions.

In Fish, since there's no built-in capitalize function, we split the string into words, capitalize the first char, and lowercase the rest. This not only capitalizes English texts but can also handle texts with special characters due to Unicode support.

Alternatives for string capitalization include using external programs like `awk`, `sed`, or writing a fish function (as shown above). You could also capitalize strings within an editor that supports find-and-replace with regex, or use a programming language that has in-built string capitalization functions like Python or JavaScript.

## See Also (Zobacz też):
- Fish documentation on string manipulation: [https://fishshell.com/docs/current/](https://fishshell.com/docs/current/)
- Unicode support in Fish: [https://fishshell.com/docs/current/index.html#unicode](https://fishshell.com/docs/current/index.html#unicode)
- Bash string manipulation for comparison: [https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
