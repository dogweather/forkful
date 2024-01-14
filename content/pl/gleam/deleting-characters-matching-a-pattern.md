---
title:                "Gleam: Usuwanie znaków pasujących do wzorca"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Często podczas pisania kodu w języku Gleam, możemy natknąć się na potrzebę usunięcia znaków odpowiadających określonemu wzorcowi. To może być ważne w celu czyszczenia danych wejściowych lub filtrowania danych w celu uzyskania odpowiednich wyników. W tym artykule dowiesz się, jak łatwo usuwać znaki w języku Gleam.

## Jak to zrobić

```Gleam
import gleam/regex

let input = "Cześć, ta wiadomość będzie zawierać @tagi i *słowa~ do~usunięcia*."
let output = regex.replace(input, "\\W", "")
// output: "Cześćtawiadomośćbędziezawieraćtagiisłowadousunięcia"
```

W powyższym kodzie, importujemy bibliotekę `gleam/regex` i tworzymy zmienną `input` zawierającą nasz tekst do obróbki. Następnie używamy funkcji `replace` z biblioteki `regex`, która przyjmuje dwa argumenty: tekst do przetworzenia oraz wzorzec, który ma zostać usunięty. W tym przypadku użyliśmy `\\W` jako wzorca, co oznacza, że wszystkie znaki nie będące literami lub cyframi zostaną usunięte. Na końcu, przechowujemy wynik w zmiennej `output` i możemy go wyświetlić lub wykorzystać w dalszej części naszego kodu.

## Wnikliwe spojrzenie

Funkcja `replace` z biblioteki `gleam/regex` wykorzystuje wyrażenia regularne (regex) do dopasowania i usuwania określonych znaków. W przykładzie powyżej użyliśmy `\\W`, ale można również wykorzystać inne wyrażenia regularne, takie jak `\\d` do usuwania cyfr lub `\\s` do usuwania spacji. Możliwości są nieograniczone, a użycie wyrażeń regularnych pozwala dokładnie dostosować, które znaki chcemy usunąć.

## Zobacz również

- Dokumentacja biblioteki `gleam/regex`: [https://gleam.run/modules/regex/](https://gleam.run/modules/regex/)
- Przykładowe wyrażenia regularne: [https://www.regular-expressions.info/examples.html](https://www.regular-expressions.info/examples.html)
- Wideo tutorial o wyrażeniach regularnych w języku Gleam: [https://www.youtube.com/watch?v=S981LirFApA](https://www.youtube.com/watch?v=S981LirFApA)