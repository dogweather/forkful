---
title:    "Gleam: Łączenie ciągów znaków"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Konkatenacja (łączenie) ciągów znaków jest jednym z podstawowych zadań w programowaniu. Jest to proces łączenia wielu ciągów znaków w jeden, co pozwala na tworzenie bardziej złożonych wyrażeń i przetwarzanie danych w bardziej czytelny sposób.

## Jak to zrobić

W języku Gleam, konkatenacja ciągów znaków odbywa się przy użyciu operatora `++`, który umożliwia dodawanie ciągów znaków do siebie. Przykładowo, jeśli mamy dwa ciągi znaków `Hello` i `world`, to po wykonaniu operacji `Hello ++ " " ++ world` otrzymamy wynik `Hello world`.

```Gleam
let text = "Witaj " ++ "świecie"
```

W powyższym przykładzie zmienna `text` będzie przechowywać ciąg znaków "Witaj świecie". W przypadku gdy chcemy dodać do siebie więcej niż dwa ciągi znaków, możemy po prostu kontynuować korzystając z operatora `++`.

```Gleam
let hello = "Cześć "
let name = "Jan"
let sentence = hello ++ name ++ ". Jak się masz?"
```

W powyższym przykładzie zmienna `sentence` będzie przechowywać ciąg znaków "Cześć Jan. Jak się masz?"

## Głębsza analiza

Podczas konkatenacji ciągów znaków, język Gleam automatycznie wykrywa typ zmiennej, na podstawie której dokonywana jest operacja. Jeśli jedna ze zmiennych jest typu `String`, to cały wynik będzie również typu `String`.

Ponadto, język Gleam oferuje również funkcję `String.concat`, która pozwala na konkatenację dowolnej liczby ciągów znaków podanych w liście.

```Gleam
let greetings = ["Cześć", " ", "Jak", " ", "się", " ", "masz?"]
let sentence = String.concat(greetings)
```

W powyższym przykładzie, zmienna `sentence` również będzie przechowywać ciąg znaków "Cześć Jak się masz?".

## Zobacz też

- Dokumentacja języka Gleam: https://gleam.run/documentation/
- Wprowadzenie do konkatenacji w języku Gleam: https://gleam.run/documentation/guides/strings/
- Przykładowe programy w języku Gleam: https://github.com/gleam-lang/gleam/tree/master/examples

*Tekst napisany dla Polaków, ale bez problemu będzie zrozumiały dla każdego, kto zna język polski.*