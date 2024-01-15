---
title:                "Łączenie ciągów znaków"
html_title:           "Gleam: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego powinieneś używać konkatenacji (łączenia) strings w programowaniu? W tym artykule dowiesz się, dlaczego jest to ważna umiejętność i jak jej użyć w języku Gleam.

## Jak To Zrobić

Aby połączyć dwa lub więcej stringów w Gleam, użyj funkcji `string.concat/2`. Oto przykładowy kod, który zobrazuje to w praktyce:

```Gleam
let string1 = "Programowanie"
let string2 = "jest"
let string3 = "super!"

let concat_strings = string.concat(string1, string2, string3)

io.println(concat_strings)  // Wypisze "Programowanie jest super!"
```

Możesz również użyć tej samej funkcji, aby połączyć tablicę stringów w jeden:

```Gleam
let strings = ["Gleam", "to", "język", "programowania"]

let concat_strings = string.concat(strings)

io.println(concat_strings)  // Wypisze "Gleam to język programowania"
```

## Ciekawostki

Pamiętaj, że funkcja `string.concat/2` jest dostępna tylko w module `string`, więc musisz importować moduł przed jej użyciem.

Możesz również użyć operatora `++` do konkatenacji dwóch stringów lub dwóch tablic stringów. Jednakże, dla bardziej czytelnego kodu, zaleca się używanie funkcji `string.concat/2`.

## Zobacz Również

- Dokumentacja Gleam o funkcji `string.concat/2`: https://gleam.run/modules/string#concat
- Inny artykuł o konkatenacji stringów w Gleam: https://medium.com/gleam-lang/concatenating-strings-in-gleam-c2ec27a5c119
- Przykłady zastosowania konkatenacji stringów w projektach w języku Gleam: https://github.com/search?q=language%3Agleam+string+concat&type=Code