---
title:                "Pisanie testów"
html_title:           "Gleam: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Co i Po co?

Testowanie kodu to proces sprawdzania, czy nasz program działa zgodnie z oczekiwaniami i nie zawiera błędów. Jest to nieodłączna część pracy każdego programisty, ponieważ pozwala nam uniknąć nieprzewidzianych sytuacji i ułatwia odnajdywanie i naprawianie błędów w kodzie.

## Jak to zrobić:

```Gleam
// Przykładowa funkcja przyjmująca listę liczb i zwracająca ich sumę
pub fn suma(lista: List(Int)) -> Int {
  let gdy_lista_pusta = ctor -> 0
  let gdy_liczba :: reszta = lista -> liczba + suma(reszta)
  wynik
}

// Przykładowe dane wejściowe i spodziewany wynik
let dane_wejsciowe = [3, 5, 2]
let oczekiwany_wynik = 10

// Wywołanie funkcji i porównanie wyników
assert suma(dane_wejsciowe) = oczekiwany_wynik
```

## Głębsze zagadnienia:

Testowanie kodu stało się nieodłączną częścią programowania wraz z rozwojem metodyk agile i test-driven development. W Gleam istnieje wiele narzędzi i bibliotek do pisania testów, na przykład ```gleam_testing``` oraz wbudowane makra takie jak ```assert``` i ```expect```. Alternatywnymi metodami testowania są na przykład manualne testy czy analiza statyczna kodu. W Gleam możliwe jest również pisanie testów jednostkowych, integracyjnych oraz akceptacyjnych.

## Zobacz także:

- [Dokumentacja gleam_testing](https://gleam.run/testing/)
- [Przykładowe testy w repozytorium Gleam](https://github.com/gleam-lang/gleam/tree/master/examples/testing)