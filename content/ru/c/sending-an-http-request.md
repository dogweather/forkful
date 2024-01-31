---
title:                "Отправка HTTP-запроса"
date:                  2024-01-29T00:03:28.885880-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c/sending-an-http-request.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Отправка HTTP-запроса - это способ, которым ваша программа запрашивает данные или отправляет данные на веб-сервер. Программисты используют это для взаимодействия с API, получения веб-контента или общения с другими сервисами.

## Как это сделать:

В C мы будем использовать `libcurl` для этой задачи. Это мощная библиотека для передачи данных с URL. Сначала установите `libcurl`. В системах на базе Debian вы можете использовать `sudo apt-get install libcurl4-openssl-dev`.

Вот пример для создания простого GET-запроса:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        
        /* Выполнение запроса, res получит код возврата */ 
        res = curl_easy_perform(curl);
        
        /* Проверка на ошибки */ 
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));
        
        /* всегда выполнять очистку */ 
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Запустив это, вы не увидите видимый вывод, так как мы не обрабатываем ответ, но это извлечет содержимое с `http://example.com`.

## Подробнее

`libcurl` был создан в 1997 году и стал идти-библиотекой для программистов на C. Он поддерживает огромное количество протоколов, не только HTTP. Альтернативы HTTP в C могут включать написание собственной реализации, но это тернистый путь через программирование сокетов и сложные RFC.

`libcurl` удобен, потому что он берет на себя все мелкие детали, такие как согласование протоколов, обработка ошибок и передача данных. К тому же он кросс-платформенный - используйте тот же код на Linux, Windows, Mac, как хотите.

Помните, что `libcurl` по умолчанию использует синхронное API, которое может блокировать ваш основной поток. Если вы создаете что-то, где это имеет значение, вам, возможно, придется погрузиться в многопоточность или набор асинхронных функций `curl_multi_*`.

## Смотрите также

- Официальный сайт libcurl для документации и примеров: [https://curl.se/libcurl/](https://curl.se/libcurl/)
- Детали протокола HTTP для общих знаний: [https://www.ietf.org/rfc/rfc2616.txt](https://www.ietf.org/rfc/rfc2616.txt)
- Для более широкого взгляда на сетевое программирование на C: [Beej's Guide to Network Programming](https://beej.us/guide/bgnet/)
