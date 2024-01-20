---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що та для чого?

Завантаження веб-сторінки включає отримання вмісту веб-сторінки через Інтернет. Програмісти роблять це, щоб отримати дані з веб-сайтів або виконати операції на стороні сервера.

## Як це зробити:

В Elm ми можемо використовувати модуль `Http` для завантаження веб-сторінки. Ось приклад:

```Elm
import Http
import Json.Decode as Decode

getWebsiteContent : String -> Cmd Msg
getWebsiteContent url =
    Http.get
        { url = url
        , expect = Http.expectString GotWebsiteContent
        }

type Msg = GotWebsiteContent (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotWebsiteContent result ->
            case result of
                Ok response ->
                    ...

    Http.send GotWebsiteContent (getWebsiteContent "http://example.com")
```

## Більше деталей:

### Історичний контекст
Elm був розроблений Evan Czaplicki у 2012 році як безпечна, функциональна мова програмування для веб-розробки, зокрема для роботи з HTTP-запитами.

### Альтернативи
У Elm основний спосіб роботи з HTTP - це через використання модуля `Http`. Однак, є інші JavaScript-бібліотеки, такі як `fetch` або `axios`, які можна використовувати через порти.

### Деталі реалізації
Зверніть увагу, що Elm використовує свій власний тип `Http.Error` для обробки помилок,