---
title:                "Отправка HTTP-запроса с базовой аутентификацией"
date:                  2024-01-29T00:02:57.593024-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса с базовой аутентификацией"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Отправка HTTP-запроса с базовой аутентификацией включает добавление заголовка с именем пользователя и паролем для доступа к защищенным ресурсам. Программисты делают это для взаимодействия с веб-сервисами, которые требуют учетных данных для работы.

## Как это сделать:
Для отправки HTTP-запроса с базовой аутентификацией на языке C обычно используется библиотека, например, libcurl. Вот короткий пример:

```c
#include <stdio.h>
#include <curl/curl.h>

int main() {
    CURL *curl = curl_easy_init();
    if(curl) {
        // Установите URL, который собирается получить наш POST-запрос
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/resource");
        
        // Установите учетные данные базовой аутентификации
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");
        
        // Выполните запрос
        CURLcode res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        }
        
        // Завершение работы
        curl_easy_cleanup(curl);
    }
    
    return 0;
}
```

Вывод будет зависеть от ответа сервера.

## Глубокое погружение
Отправка HTTP-запроса с базовой аутентификацией является довольно старым способом контроля доступа к веб-ресурсам. Разработанный в начале существования интернета, это не самый безопасный метод, поскольку учетные данные кодируются с помощью base64, а не шифруются.

Теперь для лучшей безопасности рекомендуются альтернативы, такие как OAuth и ключи API. Однако базовая аутентификация все еще полезна для простых скриптов или внутренних инструментов, где эти риски приемлемы.

Реализация обычно выполняется с помощью библиотек вроде libcurl или собственного программирования сокетов, если вам нужен больший контроль. Заголовки базовой аутентификации можно конструировать вручную, но это утомительно и склонно к ошибкам, поэтому библиотеки - это лучший выбор.

## Смотрите также
- Документация библиотеки cURL: https://curl.haxx.se/libcurl/c/
- RFC 7617, Схема базовой аутентификации HTTP: https://tools.ietf.org/html/rfc7617
- Веб-документация MDN об аутентификации HTTP: https://developer.mozilla.org/ru/docs/Web/HTTP/Authentication
- Введение в OAuth: https://oauth.net/
