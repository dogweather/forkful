---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Aktualna Data w Elm: Jak i Dlaczego?

## Czym Jest i Dlaczego?

Pobieranie aktualnej daty oznacza odczytanie systemowego zegara w wyniku czego otrzymujemy bieżącą datę i czas. Programiści robią to w celu zapisywania strumienia zdarzeń, trackowania czasu rzeczywistego albo generowania unikalnych timestampów.

## Jak to zrobić:

Elm oferuje wbudowany moduł `Time`. Używamy funkcji `now` do odczytania aktualnej daty i czasu. Poniżej przykład w kodzie:

```Elm
import Time

main =
    Time.now 
    |> Task.perform DataReceived
```

Zwróć uwagę, że `Time.now` zwraca Task,a nie bezpośrednio datę i czas. W Elm, operacje wejścia/wyjścia są obsługiwane jako zadania ("tasks"), które są niemutowalne i efekty boczne są definiowane razem z danymi dla czystych funkcji.

Wynik może zostać następnie przetworzony przez `Task.perform`, który spowoduje jego wykonanie.

## Zagłębienie się w szczegóły:

Elm to język programowania stworzony przez Evana Spyzgla w 2012 roku. Dzięki swojej czystej, niemutowalnej i funkcyjnej naturze, doradza się używanie Tasków do operacji I/O, które mogą wpływać na stan systemu, takich jak odczytanie aktualnej daty i czasu.

Istnieje kilka alternatyw sposobu uzyskania aktualnej daty i czasu. Możemy użyć funkcji `Time.every` do utworzenia sygnału, który będzie emitowany co pewien określony interwał czasu.

```Elm
import Time
import Signal

main = 
    let
        tick = Time.every Time.second
    in
        Signal.map second tick
```

Podczas implementacji pamiętaj, że Elm jest czystym językiem funkcyjnym – co oznacza, że nie możemy bezpośrednio zmodyfikować stanów, tak jak w większości języków obiektowych. Zamiast tego, następuje transformacja stanu z jednej postaci do drugiej.

## Zobacz więcej:

- Dokumentacja Elm `Time` module: [Elm Time](http://package.elm-lang.org/packages/elm-lang/core/latest/Time)
- Elm Architecture Tutorial: [Elm Architecture](https://guide.elm-lang.org/architecture/)