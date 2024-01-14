---
title:    "Elm: Uzyskiwanie aktualnej daty"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą w języku Elm, prawdopodobnie zastanawiasz się, po co w ogóle pobierać aktualną datę. Często nasi klienci wymagają aktualnego znacznika czasu, aby śledzić zmiany w swoich aplikacjach lub chcą wyświetlić aktualną datę i godzinę na ich interfejsie użytkownika. W tym artykule pokażę Ci, jak możesz łatwo pobrać aktualną datę w języku Elm.

## Jak to zrobić

Pobranie aktualnej daty w języku Elm jest bardzo proste. Wystarczy użyć wbudowanej funkcji `Time.now` i przekazać jej odpowiednią strefę czasową. Poniżej znajduje się przykładowy kod:

```Elm
import Time

currentTime : Time.Time
currentTime = Time.now Time.utc

-- lub w innej strefie czasowej, np. dla Polski
currentTime = Time.now Time.zone

-- możesz również użyć funkcji `Time.nowInZone` i przekazać jej strefę czasową jako argument
currentTime = Time.nowInZone "Europe/Warsaw"
```

Kod ten zwróci aktualną datę w wybranej strefie czasowej. W ten sposób możesz łatwo skonstruować własne funkcje, które wykorzystują aktualną datę do różnych celów.

## Głębszy zanurzenie

Jeśli chcesz lepiej zrozumieć, jak działa funkcja `Time.now` w języku Elm, możesz zajrzeć do dokumentacji języka. Tam znajdziesz więcej informacji na temat stref czasowych oraz innych funkcji związanych z czasem, takich jak `Time.fromMillis` czy `Time.millisSinceEpoch`, które pozwalają na manipulację czasem w bardziej wyrafinowany sposób.

## Zobacz także
- [Dokumentacja języka Elm](https://elm-lang.org/docs)
- [Dokumentacja modułu `Time` w języku Elm](https://package.elm-lang.org/packages/elm/core/latest/Time)

Dzięki tym prostym wskazówkom będziesz w stanie łatwo pobierać aktualną datę w języku Elm i wykorzystywać ją w swoich aplikacjach. Daj znać, w jaki sposób wykorzystujesz tę funkcję w swoim kodzie i udostępnij ten artykuł innym programistom, którzy też mogą potrzebować pomocy z tym zagadnieniem.