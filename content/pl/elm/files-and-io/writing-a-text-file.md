---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:01.014806-07:00
description: "Tworzenie pliku tekstowego w Elm polega na tworzeniu i zapisywaniu danych\
  \ tekstowych do pliku z aplikacji Elm. Programi\u015Bci cz\u0119sto musz\u0105 generowa\u0107\
  \ raporty,\u2026"
lastmod: '2024-03-13T22:44:35.340196-06:00'
model: gpt-4-0125-preview
summary: Tworzenie pliku tekstowego w Elm polega na tworzeniu i zapisywaniu danych
  tekstowych do pliku z aplikacji Elm.
title: Pisanie pliku tekstowego
weight: 24
---

## Co i dlaczego?

Tworzenie pliku tekstowego w Elm polega na tworzeniu i zapisywaniu danych tekstowych do pliku z aplikacji Elm. Programiści często muszą generować raporty, logi lub eksportować dane w strukturalnym formacie tekstowym (np. JSON, CSV) do wykorzystania w innych aplikacjach lub dla celów ewidencyjnych. Jednak ze względu na architekturę Elma, która skupia się na czystości i bezpieczeństwie, bezpośrednie zapisywanie do pliku - podobnie jak wiele innych efektów ubocznych - jest obsługiwane poprzez komendy do otaczającego środowiska JavaScript.

## Jak to zrobić:

Ponieważ Elm działa w przeglądarce i jest zaprojektowany jako język programowania bez efektów ubocznych, nie ma bezpośredniego dostępu do systemu plików. Dlatego zapisywanie do pliku zwykle wiąże się z wysłaniem danych do JavaScript przez porty. Oto jak możesz to skonfigurować:

1. **Zdefiniuj moduł portu do wysyłania tekstu do JavaScriptu:**

```elm
port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

-- Zdefiniuj port do wysyłania danych tekstowych do JavaScriptu
port saveText : String -> Cmd msg

-- Główny widok
view : Html msg
view =
    div []
        [ button [ onClick (saveText "Cześć, Elm zapisuje do pliku!") ] [ text "Zapisz do pliku" ]
        ]

-- Ustawienie subskrypcji (nieużywane w tym przykładzie, ale wymagane dla modułu portu)
subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none

-- Ustawienie aplikacji
main : Program () model msg
main =
    Browser.element
        { init = \_ -> ((), Cmd.none)
        , view = \_ -> view
        , update = \_ _ -> ((), Cmd.none)
        , subscriptions = subscriptions
        }
```

2. **Zaimplementuj odpowiadający kod JavaScript:**

W pliku HTML lub module JavaScript obsłuż port aplikacji Elm do zapisywania tekstu. Możesz użyć biblioteki `FileSaver.js` do zapisywania pliku po stronie klienta lub wysłać dane na serwer do przetworzenia.

```javascript
// Zakładając, że Elm.Main.init() został już wywołany i aplikacja działa
app.ports.saveText.subscribe(function(text) {
    // Użycie FileSaver.js do zapisywania plików po stronie klienta
    var blob = new Blob([text], {type: "text/plain;charset=utf-8"});
    saveAs(blob, "przyklad.txt");
});
```

Przykładowe wyjście nie jest bezpośrednio stosowalne, ponieważ wynikiem jest utworzenie pliku, ale po kliknięciu przycisku w twojej aplikacji Elm, na twoim komputerze powinien zostać pobrany plik o nazwie "przyklad.txt" zawierający ciąg "Cześć, Elm zapisuje do pliku!".

W tym podejściu komunikacja między Elm a JavaScriptem jest kluczowa. Chociaż Elm ma na celu zawarcie jak największej części logiki twojej aplikacji, współpraca z JavaScriptem przez porty umożliwia wykonywanie zadań, takich jak zapisywanie do pliku, których Elm bezpośrednio nie obsługuje. Pamiętaj, że czystość i bezpieczeństwo Elma są wzmacniane przez ten wzorzec, co zapewnia, że twoje aplikacje Elm pozostają łatwe do utrzymania i zrozumienia, nawet kiedy wchodzą w interakcję ze skomplikowanym światem zewnętrznym.
