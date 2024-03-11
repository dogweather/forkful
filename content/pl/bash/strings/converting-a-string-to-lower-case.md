---
date: 2024-01-20 17:37:44.556610-07:00
description: "Zamiana \u0142a\u0144cucha znak\xF3w na ma\u0142e litery oznacza zmian\u0119\
  \ wszystkich du\u017Cych liter na ich ma\u0142e odpowiedniki. Programi\u015Bci robi\u0105\
  \ to cz\u0119sto, aby unikn\u0105\u0107 problem\xF3w\u2026"
lastmod: '2024-03-11T00:14:08.759024-06:00'
model: gpt-4-1106-preview
summary: "Zamiana \u0142a\u0144cucha znak\xF3w na ma\u0142e litery oznacza zmian\u0119\
  \ wszystkich du\u017Cych liter na ich ma\u0142e odpowiedniki. Programi\u015Bci robi\u0105\
  \ to cz\u0119sto, aby unikn\u0105\u0107 problem\xF3w\u2026"
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Zamiana łańcucha znaków na małe litery oznacza zmianę wszystkich dużych liter na ich małe odpowiedniki. Programiści robią to często, aby uniknąć problemów z wielkością liter, jak w przypadku porównywania haseł czy adresów email.

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
