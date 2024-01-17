---
title:                "Wycinanie podciągów"
html_title:           "Bash: Wycinanie podciągów"
simple_title:         "Wycinanie podciągów"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

W programowaniu często zdarza się, że musimy pobrać fragment tekstu z większego ciągu znaków. W Bashu jest to często nazywane "wycinaniem podciągów". Programiści używają tej techniki, aby łatwiej przetwarzać dane i dostosowywać je do swoich potrzeb.

## Jak to zrobić:

Aby wyciąć podciąg w Bashu, użyjemy polecenia ```cut```. Przedstawimy tutaj trzy przykłady tego, jak możesz z niego skorzystać:

1. Pobieranie podciągu od pierwszego do trzeciego znaku:
```Bash
echo "Hello World" | cut -c 1-3
```
Wyjście: Hel

2. Pobieranie podciągu od podanego znaku do końca:
```Bash
echo "Hello World" | cut -c 5-
```
Wyjście: o World

3. Pobieranie podciągu od podanego znaku do drugiego wystąpienia innego znaku:
```Bash
echo "Hello World" | cut -d " " -f 1
```
Wyjście: Hello

## Głębsze zagadnienia:

Wycinanie podciągów jest popularną techniką, która istnieje od dłuższego czasu. Już w programie awk, który powstał w latach 70, można było wykorzystywać podobne funkcjonalności do wycinania fragmentów tekstu. Alternatywą dla polecenia ```cut``` jest również ```sed```, które umożliwia bardziej zaawansowane operacje na tekście. Implementacja wycinania podciągów w Bashu jest nieskomplikowana i nie wymaga dużego nakładu pracy.

## Zobacz także:

[Oficjalna dokumentacja Bash](https://www.gnu.org/software/bash/manual/bash.html)

[Dokumentacja polecenia cut](https://www.linuxcommand.org/lc3_man_pages/cut1.html)

[Dokumentacja polecenia sed](https://www.gnu.org/software/sed/manual/sed.html)