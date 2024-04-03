---
date: 2024-01-20 17:50:20.262992-07:00
description: "Interpolacja string\xF3w to wstawianie warto\u015Bci zmiennych do \u0142\
  a\u0144cucha tekstowego. Programi\u015Bci robi\u0105 to, \u017Ceby dynamicznie tworzy\u0107\
  \ tekst, na przyk\u0142ad\u2026"
lastmod: '2024-03-13T22:44:35.568671-06:00'
model: gpt-4-1106-preview
summary: "Interpolacja string\xF3w to wstawianie warto\u015Bci zmiennych do \u0142\
  a\u0144cucha tekstowego."
title: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 8
---

## Co i Dlaczego?
Interpolacja stringów to wstawianie wartości zmiennych do łańcucha tekstowego. Programiści robią to, żeby dynamicznie tworzyć tekst, na przykład wiadomości czy komendy.

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
