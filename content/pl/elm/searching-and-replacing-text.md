---
title:                "Elm: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy zdarzyło Ci się już kiedyś szukać i zamieniać tekst w swoim kodzie? W programowaniu często musimy zmieniać nazwy funkcji, zmienne czy też szukać i poprawiać błędy. W tym artykule dowiesz się, dlaczego wyszukiwanie i zamienianie tekstu jest ważne dla programistów i jak to zrobić w języku Elm.

## Jak To Zrobić

W języku Elm istnieją różne sposoby na wyszukiwanie i zamienianie tekstu. W tym przykładzie wykorzystamy funkcję `String.replace` oraz `String.replaceOne` do zamiany tekstu w łańcuchu znaków.

```Elm
s = "Witaj w świecie programowania!"
newS = String.replace "Witaj" "Cześć" s
```
W powyższym przykładzie zmienna `newS` będzie przechowywać wartość "Cześć w świecie programowania!". Możesz także użyć funkcji `String.replaceOne`, aby zamienić tylko pierwsze znalezione wystąpienie tekstu.

Możesz także użyć wyrażeń regularnych w funkcji `String.replace` do dokładniejszego dopasowania tekstu, na przykład zamiany tylko wybranych słów lub liter.

```Elm
s = "Każdy programista powinien umieć programować."
newS = String.replace (Regex.regex "programi(?:ści|st)[a-z]*") "deweloperzy" s
```
W tym przykładzie zostaną zamienione wszystkie słowa zaczynające się od "programi" na słowo "deweloperzy", bez względu na końcówkę. Funkcja `Regex.regex` pozwala na skomplikowane i precyzyjne dopasowanie tekstu.

## Deep Dive

Istnieje wiele innych funkcji w języku Elm, które mogą być użyteczne podczas wyszukiwania i zamieniania tekstu. Na przykład, funkcja `String.split` pozwala na podzielenie tekstu na listę według określonego separatora. Możesz także użyć funkcji `String.join` do ponownego złączenia listy w jeden łańcuch znaków.

Funkcje `String.startsWith` i `String.endsWith` pozwala sprawdzić, czy dany tekst zaczyna się lub kończy na określoną wartość. Funkcja `String.toUpper` i `String.toLower` pozwalają na zmianę wielkości liter w tekście.

Możesz także użyć funkcji `String.trim` do usunięcia zbędnych spacji z początku i końca tekstu lub `String.contains` do sprawdzenia, czy dany tekst zawiera określony ciąg znaków.

Zauważ, że wszystkie te funkcje zwracają nowy łańcuch znaków, a nie zmieniają pierwotnej wartości. Jest to ważne, ponieważ język Elm jest funkcyjny i unika zmiany wartości zmiennych w miejscu.

## Zobacz także

- [Dokumentacja języka Elm: String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Wprowadzenie do programowania funkcyjnego w języku Elm](https://www.freecodecamp.org/news/an-introduction-to-functional-programming-with-elm/)
- [Praktyczne użycie wyrażeń regularnych w Elm](https://www.degoes.net/articles/parse-time)