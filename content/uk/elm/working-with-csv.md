---
title:                "Робота з CSV файлами"
html_title:           "Arduino: Робота з CSV файлами"
simple_title:         "Робота з CSV файлами"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Що і чому?
Обробка CSV - це робота з текстовими файлами, що містять дані у форматі, де значення розділені комами. Програмісти зазвичай роблять це для імпорту, експорту та аналізу даних, бо це універсальний формат, який легко використовувати та інтегрувати.

## Як це зробити:
Elm не має вбудованої підтримки для читання CSV, але можна використовувати зовнішні бібліотеки чи власні парсери. Тут ми використовуємо простий парсер:

```Elm
parseCsv : String -> List (List String)
parseCsv input =
    input
        |> String.split "\n"
        |> List.filter (\line -> String.length line > 0)
        |> List.map (String.split ",")

sampleCsv = """
name,age,city
Alice,30,New York
Bob,25,Los Angeles
"""

main =
    parseCsv sampleCsv
        |> toString
        |> text
```

Вихідний результат:
```
[["name","age","city"],["Alice","30","New York"],["Bob","25","Los Angeles"]]
```

## Поглиблений огляд
CSV (Comma-Separated Values) започатковано в 1970-х, це давало змогу легко обмінюватися даними між різними програмами. Попри наявність форматів з більшими можливостями, як XML та JSON, CSV залишається популярним через свою простоту. У Elm, робота з CSV часто потребує використання JavaScript через порти чи веб API, що можуть забезпечувати розширену обробку CSV.

## Дивіться також
- Elm CSV пакети: [elm-csv](https://package.elm-lang.org/packages/lovasoa/elm-csv/latest/)
- Офіційний гайд Elm про порти (для інтеграції з JavaScript): [Elm Ports](https://guide.elm-lang.org/interop/ports.html)
- Докладніше про CSV: [RFC 4180](https://tools.ietf.org/html/rfc4180)
