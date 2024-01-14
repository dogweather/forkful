---
title:                "Gleam: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego programiści często muszą łączyć ze sobą różne ciągi znaków? Na przykład, gdy tworzymy aplikację, która musi wyświetlać imię i nazwisko użytkownika, musimy skleić te dwa ciągi znaków, aby uzyskać pełne imię i nazwisko. W tym artykule dowiesz się, dlaczego concatenation jest tak ważne w programowaniu i jak możesz tego użyć w języku Gleam.

## Jak to zrobić

Aby skleić dwa ciągi znaków w języku Gleam, musimy użyć funkcji `++` (znanej również jako operacja konkatenacji). Działa ona, łącząc dwa ciągi znaków i zwracając nowy, połączony ciąg. Oto przykładowe użycie funkcji `++`:

```Gleam
let imie = "Jan "
let nazwisko = "Kowalski"
let pelne_imie = imie ++ nazwisko
```

W tym przykładzie, `pelne_imie` jest zmienną, która zawiera połączony ciąg ze zmiennych `imie` i `nazwisko`, czyli "Jan Kowalski". Możemy również użyć funkcji `++` w celu połączenia więcej niż dwóch ciągów znaków. Na przykład:

```Gleam
let zdanie = "Dzisiaj "
let pogoda = "jest piękna"
let miejsce = "w Polsce"
let pelne_zdanie = zdanie ++ pogoda ++ miejsce
```

Teraz zmienna `pelne_zdanie` będzie zawierać ciąg "Dzisiaj jest piękna w Polsce".

## Głębsza analiza

W języku Gleam możemy także użyć funkcji `++` do konkatenacji innych typów danych, takich jak liczby lub listy. Na przykład, możemy połączyć liczbę i ciąg znaków:

```Gleam
let liczba = 2020
let tekst = " rok"
let rok = liczba ++ tekst
```

Teraz zmienna `rok` będzie zawierać ciąg "2020 rok". Język Gleam jest również silnie typowany, co oznacza, że musimy używać funkcji `++` z odpowiednimi typami danych. Jeśli spróbujemy połączyć ciąg znaków z innym typem danych, takim jak liczba, otrzymamy błąd kompilacji.

## Zobacz również

- Dokumentacja Gleam dotycząca funkcji concatenation: [https://gleam.run/documentation/std/string#concat](https://gleam.run/documentation/std/string#concat)
- Inne operatory w języku Gleam: [https://gleam.run/documentation/index#operators](https://gleam.run/documentation/index#operators)
- Tutorial języka Gleam: [https://gleam.run/documentation/tutorials/](https://gleam.run/documentation/tutorials/)