---
date: 2024-01-26 04:31:04.272253-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0412 Elm \u0432\u0438 \u043F\u0440\u0430\u0446\u044E\u0454\u0442\u0435 \u0437\
  \ XML, \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0447\
  \u0438 \u043F\u0430\u043A\u0435\u0442 `elm/xml`. \u041E\u0441\u044C \u0448\u0432\
  \u0438\u0434\u043A\u0438\u0439 \u043E\u0433\u043B\u044F\u0434 \u043F\u0430\u0440\
  \u0441\u0438\u043D\u0433\u0443 XML-\u0444\u0440\u0430\u0433\u043C\u0435\u043D\u0442\
  \u0443."
lastmod: '2024-03-13T22:44:49.189895-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Elm \u0432\u0438 \u043F\u0440\u0430\u0446\u044E\u0454\u0442\u0435\
  \ \u0437 XML, \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\
  \u044E\u0447\u0438 \u043F\u0430\u043A\u0435\u0442 `elm/xml`."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

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
