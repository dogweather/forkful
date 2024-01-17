---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Bash: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co & dlaczego?
Zamiana ciągu znaków na małe litery jest procesem, w którym wszystkie litery w ciągu zostają zamienione na ich odpowiedniki w małych literach. Programiści często wykonują tę czynność w celu ujednolicenia danych oraz ułatwienia ich przetwarzania.

## Jak to zrobić:
```Bash
# Przykładowy ciąg znaków
myString="PRZYKŁADOWY CIĄG ZNAKÓW"

# Zamiana na małe litery
echo ${myString,,}

# Wynik: przykładowy ciąg znaków
```

## Na głębsze wody:
Początki zmiany ciągu znaków na małe litery sięgają lat 60., kiedy to powstał system operacyjny Unix. Wcześniej, wyłącznie wielkie litery były używane w programowaniu, co utrudniało czytanie i edycję kodu. Alternatywą dla zamiany ciągu na małe litery jest użycie funkcji "tr", jednak nie jest to zalecane w przypadku bardziej skomplikowanych zadań. Implementacja w Bash wykorzystuje funkcję "parameter expansion" oraz "pattern substitution".

## Zobacz również:
- [Parameter Expansion](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Pattern Substitution](https://www.gnu.org/software/bash/manual/html_node/Pattern-Matching.html#Pattern-Matching)