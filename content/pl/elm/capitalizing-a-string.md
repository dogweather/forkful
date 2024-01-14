---
title:                "Elm: Zmiana tekstu na wielkie litery"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego: Inkrementacja tekstu w Elmie może być wygodną opcją dla programistów, którzy chcą szybko i łatwo zwiększyć wartość tekstu na stronie czy w aplikacji.

## Jak To Zrobić: 
W Elmie można łatwo zwiększyć wartość tekstu używając funkcji `String.toUpper`, która zwraca kopię tekstu z tym samym formatowaniem, ale wszystkie litery zmienione na duże.

```Elm
stringToCapitalize = "witaj świecie"
capitalizedString = String.toUpper stringToCapitalize
```
Wynik wyświetli "WITAJ ŚWIECIE".

## Czego Dowiedzieć Się Dokładniej: 
Istnieje również możliwość stworzenia własnej funkcji inkrementującej tekst, jeśli potrzebujemy bardziej złożonej logiki. W Elmie funkcje są pierwszej klasy, co oznacza, że mogą być przekazywane jako argumenty lub zwracane jako wartości. Możemy wykorzystać to, aby napisać funkcję `capitalize`, która działa podobnie jak `String.toUpper`:

```Elm
capitalize string = 
    String.toUpper string
```

Funkcję możemy również rozszerzyć, aby uwzględnić różne formatowania tekstu, np. zamianę pierwszej litery na dużą, a pozostałych na małe. 

```Elm
capitalize string = 
    String.toUpper (String.left 1 string) ++ String.toLower (String.dropLeft 1 string)
```

W powyższym przykładzie wykorzystujemy funkcje `String.left` i `String.dropLeft`, aby podzielić tekst na dwie części: pierwszą literę i resztę tekstu. Następnie za pomocą funkcji `++` łączymy kolejno: pierwszą literę powiększoną do wielkich liter oraz resztę tekstu zmniejszoną do małych liter.

## Zobacz także:
- [Dokumentacja Elm: String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Podstawy programowania w Elmie](https://www.learnelm.org/)
- [Inne funkcje związane z manipulacją tekstu w Elmie](https://package.elm-lang.org/packages/elm/core/latest/String#ascii)