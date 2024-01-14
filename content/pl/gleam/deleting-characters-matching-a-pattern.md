---
title:                "Gleam: Usuwanie znaków dopasowujących wzorzec"
simple_title:         "Usuwanie znaków dopasowujących wzorzec"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Polish readers may often come across the need to delete characters in a given string that match a specific pattern. This could be for reasons such as data cleanup, removing sensitive information, or transforming data for further processing. In this blog post, we will explore how to achieve this in Gleam programming language.

## Dlaczego

Często zdarza się, że musimy usunąć znaki w ciągu, które pasują do określonego wzorca. Być może jest to konieczne dla wyczyszczenia danych, usunięcia poufnych informacji lub przekształcenia danych do dalszej obróbki. W tym artykule dowiemy się, jak osiągnąć ten cel w języku programowania Gleam.

## Jak to zrobić

W języku Gleam możemy użyć funkcji `String.replace`, która pozwala na podanie wzorca i zastępczej wartości do usunięcia pasujących znaków. Na przykład, jeśli chcemy usunąć wszystkie cyfry z ciągu, możemy użyć następującego kodu:

```Gleam
import gleam/re

let str = "Abc123Def"
let cleaned_str = String.replace(re.compile("\\d+"), "", str)
```

W powyższym przykładzie, używamy funkcji `re.compile` z pakietu `gleam/re`, aby utworzyć wzorzec wyszukiwania. Następnie przekazujemy ten wzorzec do funkcji `String.replace`, wraz z wartością pustą jako zastępczą. Otrzymujemy wynikowy ciąg, w którym wszystkie cyfry zostały usunięte.

### Przykład 1:

Wejście: "Abc123Def"
Wynik: "AbcDef"

### Przykład 2:

Wejście: "123SomeWord456"
Wynik: "SomeWord"

## Deep Dive

Funkcja `String.replace` wykorzystuje moduł `std.regex` do dopasowywania wzorca w ciągu. Możemy więc wykorzystać bogatą składnię tego modułu do bardziej zaawansowanych operacji usuwania znaków. Na przykład, możemy użyć specjalnych wyrażeń regularnych, takich jak grupowanie i odwrotna odwrotność, aby bardziej precyzyjnie określić, które znaki powinny zostać usunięte.

## Zobacz również
- Dokumentacja `gleam/re`: https://gleam.run/modules/gleam_re/latest/
- Dokumentacja `std.regex`: https://gleam.run/modules/gleam_std_regex/latest/