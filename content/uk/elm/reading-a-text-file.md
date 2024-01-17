---
title:                "Читання текстового файлу"
html_title:           "Elm: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що & Чому?

Читання текстового файлу - це процес отримання текстової інформації з файлу на комп'ютері. Програмісти виконують цю дію, щоб отримати доступ до даних, які можуть бути використані для подальшого аналізу або обробки.

## Як зробити:

```Elm
import File exposing (readAsText)
import Json.Decode exposing (decodeString)
import Http exposing (get, string)
import String
import Task exposing (attempt)

readFile : String -> Task Http.Error String
readFile filePath =
  get filePath string

decodeContents : String -> Result String a
decodeContents contents =
  case String.split "," contents of
    Right values ->
      Ok values
    Left e ->
      Err e

convertToTask : Task Http.Error String -> Task Http.Error (Result String a)
convertToTask task =
  Task.map decodeContents task

fileTask : Task Http.Error (Result String a)
fileTask =
  readFile "sample.txt"
    |> Task.andThen convertToTask

main =
  case attempt (\_ -> Decode.wrap String.fromString fileTask) of
    Err err ->
      error (show err)

    Ok task ->
      task |> Task.attempt (\_ -> IO.succeed "File read successfully: " ++ (toString task))
```

## Глибоке занурення:

Історичний контекст: ще з початків програмування, читання текстових файлів було необхідною частиною роботи з даними. Але з впровадженням графічних інтерфейсів та розширенням інтернету, цей процес став менш потрібнею дією.

Альтернативи: нативні функції зчитування файлів існують у багатьох мовах програмування, включаючи C ++ та Java. Але у Elm є вбудовані функції, які спрощують читання текстових файлів.

Деталі реалізації: функція `readAsText` використовує вбудований `File` модуль у Elm для зчитування та повернення текстових даних з файлу. Навіть якщо файл не існує, функція поверне порожній рядок замість повідомлення про помилку.

## Дивись також:

- Elm документація про зчитування файлів: https://package.elm-lang.org/packages/elm/file/latest/File
- Різні способи читання файлів у мові програмування Elm: https://discourse.elm-lang.org/t/reading-files-in-elm/4507/2 
- Відео покрокового процесу читання файлів в Elm: https://www.youtube.com/watch?v=BeV_hQHi6pY