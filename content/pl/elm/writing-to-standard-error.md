---
title:                "Pisanie do standardowego błędu"
html_title:           "Arduino: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
W Elm nie jest tak prosto pisać do standard error, jak w niektórych językach. Kiedy chcesz, żeby Twoje błędy były widoczne, używasz `Debug.log` albo flag przy kompilacji do debugowania. To pomaga oddzielić błędy od normalnych danych wyjściowych.

## Jak to zrobić:
Elm nie pozwala bezpośrednio pisać do stderr, ale możesz logować błędy używając `Debug.log`. Pamiętaj, żeby to usunąć przed wdrożeniem na produkcję.

```Elm
import Html
import Debug

main =
  let
    _ = Debug.log "Error Message" "Coś poszło nie tak"
  in
  Html.text "Sprawdź konsolę dla błędów"
```

Przykładowe wyjście w konsoli:
```
"Error Message: Coś poszło nie tak"
```

## Pogłębienie wiedzy
Elm skupia się na poprawności aplikacji, stąd brak bezpośredniego pisania do stderr. W przeszłości, bez takich mechanizmów jak `Debug.log`, błędy były trudniejsze do wyśledzenia. Alternatywą jest używanie JavaScript interop (Ports) do wysyłania wiadomości do JS, gdzie możesz użyć `console.error`. Elm zachęca do pisania kodu, który jest wolny od runtime errors, wiec mechanizmy debugowania bywają ograniczone.

## Zobacz także
- Elm Guide na temat debugowania: https://guide.elm-lang.org/debugging/
- Elm package documentation `Debug`: https://package.elm-lang.org/packages/elm/core/latest/Debug
- Elm discourse na temat praktyk debugowania: https://discourse.elm-lang.org/
