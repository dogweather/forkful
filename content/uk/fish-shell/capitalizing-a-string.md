---
title:                "Перетворення рядка на великі літери"
html_title:           "Arduino: Перетворення рядка на великі літери"
simple_title:         "Перетворення рядка на великі літери"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Навіщо?)
Capitalizing a string means turning the first character of every word to uppercase. Programmers do this for formatting consistency, like making titles look uniform.

## How to: (Як це зробити:)
In Fish Shell, you don't have a built-in command to capitalize strings, but you can achieve it with a combination of string manipulation commands.

```Fish Shell
function capitalize
    for word in $argv
        echo -n (string sub -l 1 -- $word | string upper)(string sub -s 2 -- $word)" "
    end
    echo
end

# Usage:
set sentence "hello world from fish shell"
capitalize $sentence
```

Output: 
```
Hello World From Fish Shell
```

## Deep Dive (Поглиблений Розгляд)
Fish Shell doesn't have out-of-the-box functions for string capitalization, which is common in scripting languages like Python. Historically, Unix shells focused on file and process management, not text processing. However, Fish is equipped with robust string functions that let you build your own solutions.

Another way to achieve capitalization is using `awk` within Fish:

```Fish Shell
echo "hello world from fish shell" | awk '{for(i=1;i<=NF;i++) $i=toupper(substr($i,1,1)) substr($i,2)} 1'
```

This one-liner leverages Unix's text-processing capability instead of Fish functions.

## See Also (Дивіться Також)
If you're curious to expand your Fish Shell scripting skills, check out:

- Fish Shell Documentation on String: https://fishshell.com/docs/current/cmds/string.html
- AWK Programming Language: https://www.gnu.org/software/gawk/manual/
- UNIX Text Processing with `sed` and `awk`: http://shop.oreilly.com/product/9780937175679.do