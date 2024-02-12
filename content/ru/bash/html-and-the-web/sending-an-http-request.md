---
title:                "Отправка HTTP-запроса"
aliases:
- ru/bash/sending-an-http-request.md
date:                  2024-01-29T00:02:22.089157-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/bash/sending-an-http-request.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Отправка HTTP-запроса - это способ связи с веб-серверами для получения данных или отправки форм. Программисты делают это для взаимодействия с веб-сервисами, API или для автоматизации задач, связанных с веб-контентом.

## Как:

Bash может использовать такие инструменты, как `curl` или `wget` для HTTP-запросов. Вот короткий пример с использованием `curl`.

```Bash
# Получить содержимое веб-страницы
curl https://example.com

# Отправить данные на сервер
curl -d "param1=value1&param2=value2" -X POST https://example.com/post-endpoint

# Включить заголовки в GET-запрос
curl -H "Content-Type: application/json" https://example.com
```

Пример ответа `curl`:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Подробнее

HTTP-запросы существуют с начала 90-х годов и являются основой веб-коммуникации. `curl` и `wget` - это инструменты командной строки Unix, представленные в 1996 и 1996 годах соответственно, для сетевых запросов.

`wget` обычно используется для загрузки файлов, в то время как `curl` может обрабатывать широкий спектр протоколов и предлагает больше функций, что делает его основным инструментом для отправки HTTP-запросов из командной строки.

Реализация HTTP-запроса с использованием этих инструментов включает в себя создание правильных заголовков запроса, метода (GET, POST, PUT, DELETE и т. д.) и иногда данных. Использование этих возможностей в сценариях Bash позволяет автоматизировать взаимодействие с веб-службами.

Альтернативные способы отправки HTTP-запросов в сценариях включают использование скриптовых языков, таких как Python, с библиотеками вроде `requests`, или использование инструментов вроде `httpie` для более дружелюбного интерфейса.

## См. также

- официальный сайт curl: https://curl.se/
- руководство wget: https://www.gnu.org/software/wget/manual/wget.html
- HTTPie: https://httpie.io/
- Академия Bash: https://www.bash.academy/
- Спецификации HTTP W3C: https://www.w3.org/Protocols/
