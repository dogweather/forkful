---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Usuwanie znaków pasujących do wzorca to proces usuwania określonych ciągów znaków z danych wejściowych za pomocą wzorca. Programiści robią to, żeby uporządkować, oczyścić i optymalizować swoje dane.

## Jak to zrobić:

```Bash
# Używamy polecenia tr do usunięcia wszystkich liter 'a' z pliku input.txt
tr -d 'a' < input.txt
```
Owynik może być na przykład taki:
```Bash
# Przykładowe wyjście; wszystkie litery 'a' zostały usunięte
lorem lpsum is simply dummy text of the printing nd typesetting industry.
```
## Dogłębna wiedza

Kiedyś, w erze przed komputerową, usuwanie znaków pasujących do wzorca odbywało się ręcznie lub za pomocą mechanicznych maszyn. Do cyfrowego procesu zobacz: wyrażenia regularne i sed. Z tych narzędzi, `tr` jest najprostsze i najbardziej wydajne do tych zastosowań. 

Prowadzi to do szybkiego przetwarzania, ale dość ograniczonego, bo nie obsługuje wzorców wieloznakowych. Proces ten działa w czasie liniowym, co oznacza, że czas potrzebny do przetworzenia wzorca rośnie wraz ze wzrostem jego długości.

## Zobacz także

1. Detailed guide on `sed` and `awk`: https://likegeeks.com/sed-linux/
2. Full tutorial on regular expressions: https://www.regular-expressions.info/tutorial.html
3. More on `tr`: https://www.geeksforgeeks.org/tr-command