---
title:                "Робота з XML"
aliases:
- uk/elm/working-with-xml.md
date:                  2024-01-26T04:31:04.272253-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/working-with-xml.md"
---

{{< edit_this_page >}}

## Що і чому?
Робота з XML означає парсинг, трансформування та генерацію XML-документів в Elm. Це робиться для взаємодії з багатьма веб-сервісами та застарілими системами, що використовують XML як формат даних.

## Як це зробити:
В Elm ви працюєте з XML, використовуючи пакет `elm/xml`. Ось швидкий огляд парсингу XML-фрагменту:

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
        -- Тут щось робимо з розшифрованою книгою
        Debug.toString book

    Err error ->
        -- Обробка помилок
        Debug.toString error
```

Приклад виводу, за умови відсутності помилок:

```Elm
"{ id = \"123\", title = \"Elm in Action\", author = \"Robin Heggelund Hansen\" }"
```

## Поглиблений огляд
XML (eXtensible Markup Language) існує з кінця 90-х, періоду, коли веб був навантажений текстом і потребував структурованого, але гнучкого способу передачі даних. Через свою многослівність та складність XML втратив деяку популярність на користь JSON. Однак XML все ще широко поширений, особливо в корпоративному середовищі або у протоколах, як-от SOAP.

Підхід Elm до XML є функціональним та типобезпечним. Використання пакету `elm/xml` означає прийняття філософії Elm щодо ясності та надійності. Що стосується парсингу, пакет надає ряд декодерів, які ви компонуєте, щоб обробити структуру XML.

Порівняно з альтернативами, як-от DOMParser в JavaScript або ElementTree в Python, метод Elm може здатися більш многослівним, але забезпечує безпеку. Відсутність помилок виконання через відсутні поля або невідповідності типів; якщо щось не так, ви отримуєте помилку на етапі компіляції.

Функції декодування `elm/xml` засновані на відображенні XML-вузлів на типи Elm. Ви створюєте декодери, які відображають форму ваших даних, забезпечуючи те, що ваше додаток Elm обробляє XML так само ретельно, як і власні внутрішні структури даних.

Генерація XML в Elm зустрічається рідше, але можлива за допомогою контрчастини `elm/xml` - `Xml.Encode`.

## Дивіться також
- Посібник Elm по JSON, який також застосовується до ментальності XML: [https://guide.elm-lang.org/interop/json.html](https://guide.elm-lang.org/interop/json.html)
- Стандарт XML від W3C для глибшого розуміння самого XML: [https://www.w3.org/XML/](https://www.w3.org/XML/)
