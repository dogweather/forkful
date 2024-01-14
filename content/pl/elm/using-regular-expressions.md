---
title:    "Elm: Używanie wyrażeń regularnych"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Dlaczego warto wykorzystywać wyrażenia regularne

Wyrażenia regularne są bardzo przydatne w programowaniu, ponieważ pozwalają łatwo i szybko przeprowadzić operacje na tekście. Mogą być wykorzystywane do walidacji danych wejściowych, wyszukiwania wzorców w tekście czy też modyfikacji łańcuchów znaków.

## Jak to zrobić

 W Elmie możemy używać wyrażeń regularnych dzięki wbudowanemu modułowi `Regex`. Przed przystąpieniem do pracy musimy dodać poniższy fragment kodu na początku pliku:

```elm
import Regex exposing (..)
```

### Wyszukiwanie wzorców w tekście
Aby znaleźć określony wzorzec w tekście, należy użyć funkcji `Regex.find` i przekazać jako argumenty wyrażenie regularne oraz tekst, w którym chcemy szukać. Przykładowy kod:

```elm
text = "Witaj, to jest przykładowy tekst"
pattern = Regex.regex "przykładowy"
Regex.find pattern text
```

W powyższym przykładzie, funkcja `Regex.find` zwróci listę wyników, w których odnaleziono wzorzec `przykładowy` w tekście.

### Walidacja danych wejściowych
Wyrażenia regularne mogą być również wykorzystane do walidacji danych wejściowych, na przykład w formularzach. Możemy sprawdzić czy przekazane przez użytkownika dane spełniają określone kryteria, na przykład czy numer telefonu został podany w poprawnym formacie. Przykładowy kod:

```elm
input = "123456789"
pattern = Regex.regex "[0-9]{9}"
Regex.contains pattern input
```

W powyższym przykładzie, funkcja `Regex.contains` zwróci wartość `True`, jeśli przekazany numer telefonu składa się z dokładnie 9 cyfr.

## Głębsze spojrzenie

Wyrażenia regularne posiadają wiele zaawansowanych możliwości i składni, na które warto się przyjrzeć. Należy również pamiętać, że w Elmie wyrażenia regularne są niezmiennicze, co oznacza, że po utworzeniu nie mogą zostać zmienione.

## Zobacz też

- Dokumentacja modułu Regex w Elm: https://package.elm-lang.org/packages/elm/regex/latest/
- Przykładowe wyrażenia regularne w Elmie: https://korban.net/programming/elm-regexp-cheatsheet/
- Wideo tutorial o wyrażeniach regularnych w Elmie: https://www.youtube.com/watch?v=OvDaxWi5gi8