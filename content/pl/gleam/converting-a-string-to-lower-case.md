---
title:                "Gleam: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Zamiana ciągu znaków na małe litery jest bardzo przydatnym narzędziem w programowaniu. Dzięki temu możemy bez problemu porównywać i sprawdzać równość tekstu bez względu na wielkość liter. Jest to także często wykorzystywane przy tworzeniu aplikacji z interfejsem użytkownika, gdzie chcemy, aby użytkownik wprowadzał tylko małe litery w pola tekstowe.

## Jak to zrobić?

W języku Gleam istnieje bardzo prosty sposób na zamianę wszystkich znaków danej ciągu na małe litery. Wystarczy wykorzystać funkcję `String.to_lower` i jako argument podać nasz ciąg znaków. Poniżej znajduje się przykład kodu:

```Gleam
let example_string = "TEKST DO ZAMIANy"
let lower_string = String.to_lower(example_string)
```

W efekcie otrzymamy wartość `tekst do zamiany` w zmiennej `lower_string`.

## Głębsze wgląd

Warto zauważyć, że funkcja `String.to_lower` nie tylko zmienia litery na małe, ale również uwzględnia polskie znaki. W związku z tym, jeśli nasz ciąg zawierałby litery `Ą`, `Ę`, `Ł` itp. zostaną one również zamienione na odpowiednie małe znaki.

Język Gleam jest także bardzo wydajny w wykonywaniu operacji na tekście, więc zamiana na małe litery nie będzie wpływać na wydajność naszego kodu.

## Zobacz również

- Dokumentacja języka Gleam: https://gleam.run/
- Przykładowy kod: https://github.com/gleam-lang/gleam/blob/master/src/gleam/string.erl