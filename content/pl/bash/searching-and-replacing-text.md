---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyszukiwanie i zamiana tekstu polega na lokalizowaniu konkretnych sekwencji znaków w tekście i ich podmienianiu. Programiści robią to, by modyfikować dane, naprawiać błędy albo dostosowywać kody do nowych wymagań.

## Jak to zrobić:

Kod poniżej demonstruje, jak używać `grep` do wyszukiwania tekstu i `sed` do zamiany tekstu w Bashu.

```Bash
# Wyszukiwanie tekstu
grep "cel" plik.txt

# Zamiana tekstu
sed 's/stary nowy/g' plik.txt
```

Jeśli plik.txt zawiera 'cel', `grep` wyświetli linie z wynikami. `sed` podmieni każde wystąpienie 'stary' na 'nowy' w plik.txt.

## Deep Dive

Metody wyszukiwania i zamiany tekstu istnieją od początków informatyki. Komendy `grep` i `sed` wywodzą się z systemu Unix z lat 70-tych, ale nadal są wykorzystywane z powodu swej efektywności i prostoty.

Alternatywą dla `grep` i `sed` może być `awk`, który jest bardziej złożony, lecz oferuje wyższy stopień kontroli. Możemy też skorzystać z wbudowanych funkcji wyszukiwania i zamiany w edytorach tekstowych jak Vim czy Emacs.

Wszystko to, co robimy za pomocą `grep` i `sed`, jest interpretowane przez interpreter shella. Interpreter odczytuje nasz kod linia po linii, interpretuje go i wykonuje. 

## Zobacz także

- Dokumentacja GNU dla `grep`: https://www.gnu.org/software/grep/manual/grep.html
- Dokumentacja GNU dla `sed`: https://www.gnu.org/software/sed/manual/sed.html
- Vim Tutor: http://www2.geog.ucl.ac.uk/~plewis/teaching/unix/vimtutor
- Emacs manual: https://www.gnu.org/software/emacs/manual/emacs.html.