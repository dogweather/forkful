---
date: 2024-01-20 17:37:44.556610-07:00
description: "How to: (Jak to zrobi\u0107:) W Bashu mo\u017Cemy u\u017Cy\u0107 kilku\
  \ prostych technik."
lastmod: '2024-04-05T21:53:36.997328-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) W Bashu mo\u017Cemy u\u017Cy\u0107 kilku prostych\
  \ technik."
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
weight: 4
---

## How to: (Jak to zrobić:)
W Bashu możemy użyć kilku prostych technik:

```Bash
# Opcja 1: Użycie wbudowanej funkcjonalności
string="Some Text"
lowercase_string="${string,,}"
echo $lowercase_string # some text

# Opcja 2: Użycie 'tr'
echo "Some Text" | tr '[:upper:]' '[:lower:]' # some text

# Opcja 3: Użycie 'awk'
echo "Some Text" | awk '{print tolower($0)}' # some text
```

## Deep Dive (W Głąb Tematu)
Zamiana tekstu na małe litery to stary, ale wciąż użyteczny trik w programowaniu. Umożliwia standardowe porównanie stringów, bez konieczności martwienia się o wielkość liter. W przeszłości programiści używali różnych narzędzi do tego celu, jak `tr` czy `awk`. Bash od wersji 4.0 wprowadził jednak wbudowaną funkcjonalność (parameter expansion), która pozwala na łatwą i szybką manipulację łańcuchami znaków - jak np. zamiana na małe litery z użyciem "${string,,}". Opcja z `awk` jest warta uwagi, gdy pracujemy na danych przepływowych, tj. w potokach.

## See Also (Zobacz Również)
- GNU `tr` manual: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- Awk w praktyce: https://www.gnu.org/software/gawk/manual/gawk.html
- Bash reference manual, szczególnie na temat 'Shell Parameter Expansion': https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
