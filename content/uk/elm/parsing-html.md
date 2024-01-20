---
title:                "Розбір HTML"
html_title:           "Arduino: Розбір HTML"
simple_title:         "Розбір HTML"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Що і для чого?

Парсинг HTML - це процес, при якому програма (парсер) розбирає HTML-код і перетворює його на зрозумілі для неї структури. За його допомогою програмісти можуть отримувати специфічну інформацію з веб-сторінок або модифікувати їх.

## Як?

Мова Elm має вбудований інструмент для парсингу HTML, візьміть цей приклад:

```Elm
import Html.Parser as Parser
import Html.Parser.Util as ParserUtil
import Char

html = "<div><h1>Elm</h1><p>Вивчаємо парсинг HTML</p></div>"

main =
  case Parser.run ParserUtil.document html of
    Ok elements ->
      String.concat (List.map show elements)

    Err message ->
      "Помилка: " ++ message
```

Результат виконання програми:

```
[Element "div" [] [Element "h1" [] [Text "Elm"],Element "p" [] [Text "Вивчаємо парсинг HTML"]]]
```

## Поглиблений огляд

Історично парсинг HTML використовувався для скрапінгу веб-сторінок, але з розвитком веб-програмування його використовують для програмного взаємодії з веб-сторінками.
Альтернативами парсингу HTML є використання API чи JSON. Однак, не всі веб-сторінки мають API, а JSON не завжди є доступний.

Elm використовує інтуїтивно зрозумілий, але потужний алгоритм парсингу, який робить впевнене і безпечне видобування даних з HTML-коду.

## Дивіться також

* [Elm HTML Parser](https://package.elm-lang.org/packages/elm/html/latest/)
* [Застосування парсерів в Elm](https://www.elm-spa.dev/guide/parsers)