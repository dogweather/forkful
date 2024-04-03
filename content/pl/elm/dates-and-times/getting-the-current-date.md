---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:19.415342-07:00
description: "Jak to zrobi\u0107: Elm obs\u0142uguje daty za pomoc\u0105 modu\u0142\
  u `Time`. Otrzymasz bie\u017C\u0105cy czas jako znacznik czasu POSIX, a nast\u0119\
  pnie przekonwertujesz go na dat\u0119."
lastmod: '2024-03-13T22:44:35.332407-06:00'
model: gpt-4-0125-preview
summary: "Elm obs\u0142uguje daty za pomoc\u0105 modu\u0142u `Time`."
title: Pobieranie aktualnej daty
weight: 29
---

## Jak to zrobić:
Elm obsługuje daty za pomocą modułu `Time`. Otrzymasz bieżący czas jako znacznik czasu POSIX, a następnie przekonwertujesz go na datę.

```Elm
import Browser
import Task
import Time

type Msg = GetCurrentTime Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetCurrentTime posixTime ->
            let
                -- Konwersja czasu POSIX na rekord daty
                date = Time.toDate posixTime
            in
            -- Tutaj odpowiednio aktualizuj swój model
            ({ model | date = date }, Cmd.none)

-- Aby zainicjować uzyskanie bieżącego czasu
getCurrentTime : Cmd Msg
getCurrentTime =
    Task.perform GetCurrentTime Time.now

-- Przykładowe wyjście:
-- date { year = 2023, month = Mar, day = 26 }
```

## Dogłębna analiza
W starszych językach webowych, pobranie daty to kod jednolinijkowy. Elm jest inny. Czyni skutki uboczne takie jak uzyskiwanie bieżącego czasu wyraźnymi dzięki Architekturze Elma. To zachęca do czystości i utrzymywalności kodu.

Alternatywy obejmują korzystanie z pakietów stron trzecich lub obsługę dat w kodzie serwera i przekazywanie ich do Elma przez flagi lub porty.

Pod względem implementacji, `Time.now` w Elmie pobiera czas jako znacznik czasu POSIX (milisekundy od epoki Unix). Jest to niezależne od strefy czasowej, a formatować możesz go według potrzeb przy użyciu funkcji z modułu `Time`.

## Zobacz również
- [Dokumentacja Elm Time](https://package.elm-lang.org/packages/elm/time/latest/)
- [Przewodnik Elma po komendach i subskrypcjach](https://guide.elm-lang.org/effects/)
