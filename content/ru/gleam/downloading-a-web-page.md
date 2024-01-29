---
title:                "Загрузка веб-страницы"
date:                  2024-01-28T23:57:27.754727-07:00
model:                 gpt-4-0125-preview
simple_title:         "Загрузка веб-страницы"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/gleam/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Скачивание веб-страницы означает получение её содержимого через HTTP. Программисты делают это для веб-скрейпинга, анализа данных или для взаимодействия с веб-сервисами.

## Как это сделать:

Давайте получим веб-страницу с помощью Gleam с использованием пакета `gleam_http`. Предположим, что `gleam_http` и `gleam_otp` уже находятся в зависимостях вашего проекта.

```gleam
import gleam/http
import gleam/httpc
import gleam/should

pub fn main() -> Result(String, Nil) {
  let response = httpc.send(http.Request(to: "http://example.com")) // Совершаем GET запрос
  should.equal(response.status, 200) // Подтверждаем, что статус-код OK
  Ok(response.body) // Возвращаем тело ответа
}

```

Пример вывода после выполнения вашего кода может выглядеть так:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## Погружение в тему

Еще в далеких временах, на заре интернета, скачивание страницы было так же просто, как подключение через telnet к порту 80. Сегодня у вас есть библиотеки и языки, такие как Gleam, которые заботятся о всех тонкостях HTTP.

Альтернативы `gleam_http` включают более низкоуровневые библиотеки или взаимодействие с другими библиотеками Erlang/Elixir с использованием возможностей интероперабельности Gleam.

Функция `gleam_http` `httpc.send()` выполняет основную работу в нашем примере. Она создана на основе модуля Erlang `httpc`, предоставляя простой API с некоторым количеством типовой безопасности и сопоставления с образцом из Gleam.

## Смотрите также

- Документация по Gleam: https://hexdocs.pm/gleam/gleam_http/
- Репозиторий `gleam_http` на GitHub: https://github.com/gleam-lang/http
- Введение в HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP
- Для глубокого изучения веб-скрейпинга ознакомьтесь с Beautiful Soup для Python: https://www.crummy.com/software/BeautifulSoup/
