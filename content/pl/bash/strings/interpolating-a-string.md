---
date: 2024-01-20 17:50:20.262992-07:00
description: "Jak to zrobi\u0107: ."
lastmod: '2024-03-13T22:44:35.568671-06:00'
model: gpt-4-1106-preview
summary: .
title: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 8
---

## Jak to zrobić:
```Bash
# Define a variable
user="Janek"

# Interpolate variable in a string
echo "Witaj, $user!"

# Sample output
Witaj, Janek!
```

Użyj podwójnych cudzysłowów, by interpolacja zadziałała. W pojedynczych cudzysłowach tekst zostanie wyświetlony dosłownie.

## Dogłębniej:
Interpolacja stringów w Bashu to standard od lat. Alternatywą jest składnia `printf`, która jest bardziej skomplikowana, ale oferuje większą kontrolę nad formatem:

```Bash
# Using printf
printf "Witaj, %s!\n" "$user"
```

Interpolacja wykorzystuje mechanizm nazwany "parameter expansion", co pozwala na różne operacje na zmiennych podczas interpolacji – na przykład ustawianie wartości domyślnych.

## Zobacz też:
- [Bash Parameter Expansion](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion)
- [Bash String Manipulation](https://www.tldp.org/LDP/abs/html/string-manipulation.html)
- [Advanced Bash-Scripting Guide](https://www.tldp.org/LDP/abs/html/)
