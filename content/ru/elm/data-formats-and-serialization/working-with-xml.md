---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:36.321363-07:00
description: "\u041A\u0430\u043A: \u0412 Elm \u0434\u043B\u044F \u0440\u0430\u0431\
  \u043E\u0442\u044B \u0441 XML \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\
  \u0442\u0441\u044F \u043F\u0430\u043A\u0435\u0442 `elm/xml`. \u0412\u043E\u0442\
  \ \u043A\u0440\u0430\u0442\u043A\u0438\u0439 \u043F\u0440\u0438\u043C\u0435\u0440\
  \ \u0440\u0430\u0437\u0431\u043E\u0440\u0430 \u0444\u0440\u0430\u0433\u043C\u0435\
  \u043D\u0442\u0430 XML."
lastmod: '2024-03-13T22:44:44.945242-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Elm \u0434\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441\
  \ XML \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442\u0441\u044F \u043F\
  \u0430\u043A\u0435\u0442 `elm/xml`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML"
weight: 40
---

## Как:
В Elm для работы с XML используется пакет `elm/xml`. Вот краткий пример разбора фрагмента XML:

```Elm
import Xml.Decode exposing (..)
import Xml.Decode.Pipeline exposing (..)

xmlString = """
<book id="123">
    <title>Elm in Action</title>
    <author>Robin Heggelund Hansen</author>
</book>
"""

type alias Book =
    { id : String
    , title : String
    , author : String
    }

bookDecoder : Decoder Book
bookDecoder =
    decode Book
        |> required "id" (attribute "id")
        |> required "title" (child "title" (content text))
        |> required "author" (child "author" (content text))

case Xml.Decode.fromString bookDecoder xmlString of
    Ok book ->
        -- Здесь что-то делаем с декодированной книгой
        Debug.toString book

    Err error ->
        -- Обработка ошибок
        Debug.toString error
```

Пример вывода, предполагая отсутствие ошибок:

```Elm
"{ id = \"123\", title = \"Elm in Action\", author = \"Robin Heggelund Hansen\" }"
```

## Подробнее
XML (eXtensible Markup Language) существует с конца 90-х, когда веб был перегружен текстом, и существовала острая необходимость в структурированном, но гибком способе передачи данных. Из-за многословности и сложности XML потерял некоторую популярность перед JSON. Тем не менее, XML до сих пор широко распространен, особенно в корпоративной среде или протоколах, таких как SOAP.

Подход Elm к XML является функциональным и типобезопасным. Использование пакета `elm/xml` означает принятие философии Elm о явности и надежности. Что касается разбора, пакет предоставляет ряд декодеров, которые вы компонуете для обработки структуры XML.

По сравнению с альтернативами, такими как DOMParser в JavaScript или ElementTree в Python, метод Elm может показаться более многословным, но он обеспечивает безопасность. Нет исключений времени выполнения из-за отсутствующих полей или несоответствия типов; если что-то не так, вы получаете ошибку на этапе компиляции.

Функции декодирования `elm/xml` основаны на сопоставлении узлов XML с типами Elm. Вы строите декодеры, которые отражают форму ваших данных, обеспечивая, чтобы ваше приложение Elm обрабатывало XML так же строго, как и свои внутренние структуры данных.

Генерация XML в Elm встречается реже, но может быть достигнута с помощью `elm/xml` и его аналога `Xml.Encode`.

## Смотрите также
- Руководство по Elm о JSON, которое также применимо к мышлению о XML: [https://guide.elm-lang.org/interop/json.html](https://guide.elm-lang.org/interop/json.html)
- Стандарт XML от W3C для более глубокого понимания XML: [https://www.w3.org/XML/](https://www.w3.org/XML/)
