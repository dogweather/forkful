---
date: 2024-01-26 01:02:26.774028-07:00
description: "Jak to zrobi\u0107: Architektura Elma nie wspiera efekt\xF3w ubocznych\
  \ takich jak logowanie \"od r\u0119ki\" \u2014 obs\u0142uguje si\u0119 je za pomoc\u0105\
  \ komend, kt\xF3re s\u0105 cz\u0119\u015Bci\u0105\u2026"
lastmod: '2024-03-13T22:44:35.328166-06:00'
model: gpt-4-1106-preview
summary: "Architektura Elma nie wspiera efekt\xF3w ubocznych takich jak logowanie\
  \ \"od r\u0119ki\" \u2014 obs\u0142uguje si\u0119 je za pomoc\u0105 komend, kt\xF3\
  re s\u0105 cz\u0119\u015Bci\u0105 architektury aplikacji."
title: "Rejestrowanie zdarze\u0144"
weight: 17
---

## Jak to zrobić:
Architektura Elma nie wspiera efektów ubocznych takich jak logowanie "od ręki" — obsługuje się je za pomocą komend, które są częścią architektury aplikacji. W celach edukacyjnych, sprawdźmy, jak można by symulować logowanie, wysyłając wiadomości do JavaScriptu przez porty.

Na początek zdefiniujesz moduł portu:

```Elm
port module Logger exposing (..)

-- Zdefiniowanie portu do wysyłania logów do JavaScript
port log : String -> Cmd msg
```

W `Main.elm` użyjesz portu `log`, aby wysłać wiadomość logu:

```Elm
import Logger exposing (log)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AnEvent ->
            -- tutaj jakieś aktualizacje twojego modelu
            ( updatedModel, log "AnEvent occurred." )

        AnotherEvent ->
            -- inne aktualizacje modelu tutaj
            ( anotherUpdatedModel, log "AnotherEvent occurred." )
```

Po stronie JavaScripta subskrybowałbyś port `log`, aby obsłużyć przychodzące wiadomości logów:

```JavaScript
var app = Elm.Main.init({ /* ... */ });

app.ports.log.subscribe(function(message) {
    console.log(message);
});
```

Przykładowe wyjście w konsoli JavaScriptu wyglądałoby następująco:

```
AnEvent occurred.
AnotherEvent occurred.
```

## Szczegółowa analiza
Tradycyjnie, w językach takich jak Python czy Java, logowanie odbywa się za pomocą biblioteki do logowania, która udostępnia proste API do logowania wiadomości na różnych poziomach, takich jak debug, info, ostrzeżenie, błąd i krytyczny.

Elm, ze swoim naciskiem na czystość i niemutowalność, nie oferuje tego typu bezpośredniego logowania, jako że wszelkiego rodzaju IO czy efekty uboczne są zarządzane w sposób wyraźny przez architekturę Elma.

Kiedy potrzeba pełnoprawnego logowania w Elm, zazwyczaj polega się na zewnętrznych narzędziach JavaScriptowych. Porty, jak pokazano powyżej, są mostem do tych narzędzi. Moduł Debug jest kolejną opcją, ale jest przeznaczony tylko do użytku deweloperskiego, a nie do logowania produkcyjnego.

Oprócz portów, programiści często korzystają z komunikatów kompilatora Elma i udogodnień do debugowania w czasie wykonania, takich jak `Debug.log`, które można wstawić do kodu, aby śledzić wartości. Otacza ono wyrażenie i loguje jego wynik do konsoli następująco:

```Elm
view model =
    Debug.log "Model Debug" model
    -- tutaj twój kod widoku
```

Jednakże to również nie jest przeznaczone do produkcji. Narzędzia takie jak elm-logger dostarczają pewnych abstrakcji nad portami dla logowania, choć te również są przeznaczone bardziej do użytku deweloperskiego niż produkcyjnego.

## Zobacz również
- Porty w Elm: https://guide.elm-lang.org/interop/ports.html
- Elm `Debug`: https://package.elm-lang.org/packages/elm/core/latest/Debug
- Dyskusja o logowaniu w Elm: https://discourse.elm-lang.org/t/elm-and-logging/546
- API konsoli JavaScript: https://developer.mozilla.org/en-US/docs/Web/API/Console
- Pakiet elm-logger: https://package.elm-lang.org/packages/arkgil/elm-logger/latest/
