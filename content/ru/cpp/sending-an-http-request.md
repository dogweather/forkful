---
title:                "Отправка HTTP-запроса"
date:                  2024-01-29T00:02:33.254442-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса"

category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/cpp/sending-an-http-request.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Отправка HTTP-запроса извлекает данные с веб-сервера. Программисты делают это для взаимодействия с веб-сервисами, сбора информации или общения между системами.

## Как это сделать:

```C++
#include <iostream>
#include <cpr/cpr.h> // Убедитесь, что библиотека CPR предварительно установлена

int main() {
    cpr::Response r = cpr::Get(cpr::Url{"http://httpbin.org/get"});
    std::cout << r.text << std::endl; // Выводит тело ответа
    return 0;
}
```

Пример вывода:
```json
{
  "args": {},
  "headers": {
    "Accept": "*/*",
    "Host": "httpbin.org",
    "User-Agent": "curl/7.64.1"
  },
  "origin": "0.0.0.0",
  "url": "https://httpbin.org/get"
}
```

## Подробнее
HTTP-запросы являются ключевыми с момента появления веба; они следуют модели клиент-сервер. До появления библиотек C++, таких как CPR, отправка HTTP-запросов обычно подразумевала непосредственное использование `libcurl` или интеграцию с другим языком, более приспособленным для веб-коммуникации.

CPR, что означает C++ Requests, это простая в использовании оболочка, вдохновленная модулем `requests` Python. Она абстрагирует многие сложности `libcurl`. Существуют и альтернативы, такие как Boost.Beast для операций с HTTP/S на более низком уровне или библиотеки POCO, предлагающие переносимость.

Погружаясь в детали, отправка HTTP-запроса включает в себя настройку TCP-соединения, форматирование запроса в соответствии с протоколом HTTP, а затем разбор ответа. Добиться этого с нуля непросто из-за обработки ошибок, сложностей версий HTTP и вопросов безопасности.

## Смотрите также

- CPR Github Репозиторий: https://github.com/libcpr/cpr
- Документация `libcurl` для более продвинутого использования: https://curl.se/libcurl/
- Официальная документация Boost.Beast: https://www.boost.org/doc/libs/release/libs/beast/
- Документация библиотек C++ POCO: https://pocoproject.org/docs/
