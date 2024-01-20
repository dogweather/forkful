---
title:                "Читання текстового файлу"
html_title:           "Arduino: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що і чому?
Читання текстового файлу - це процес, під час якого програма відкриває та читає вміст текстового документу. Програмісти роблять це, щоб вивчити дані, збережені у файлі, або для програмного маніпулювання цими даними.

## Як зробити:
Elm не підтримує читання з файлів напряму, але ви можете обробити вхідні дані через порти. Нижче наведено приклад коду.

```Elm
port module Main exposing (..)

import Html exposing (..)

type alias Model = 
    { fileContent: String
    }

init : Model
init =
    { fileContent = ""
    }

-- Port binding to read file
port readFile : (() -> msg) -> Sub msg

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
```

## Поглиблено:
Починаючи з Elm 0.19, мову Elm обмежено таким чином, щоб вона була більш безпечною і прогнозованою. Це означає, що немає нативної підтримки для таких речей, як читання файлів. Замість цього Elm використовує порти для взаємодії з JavaScript для таких операцій. Хоча це може бути незручно, це запобігає багатьом типам помилок.

## Дивись також:
1. [Elm Guide on Interacting with JavaScript](https://guide.elm-lang.org/interop/)
2. [Elm package for File Handling](https://package.elm-lang.org/packages/elm/file/latest/)