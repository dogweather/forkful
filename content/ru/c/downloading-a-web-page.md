---
title:                "Загрузка веб-страницы"
date:                  2024-01-28T23:57:57.164834-07:00
model:                 gpt-4-0125-preview
simple_title:         "Загрузка веб-страницы"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Скачивание веб-страницы означает получение её HTML-содержимого с веб-сервера, на котором она размещена. Программисты делают это для обработки, анализа или взаимодействия с данными веб-страницы в оффлайн-режиме.

## Как это сделать:
```C
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t written = fwrite(ptr, size, nmemb, stream);
    return written;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://example.com";
    char outfilename[FILENAME_MAX] = "downloaded_page.html";

    curl = curl_easy_init();
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        
        res = curl_easy_perform(curl);
        /* Проверка на ошибки */
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() не удалось: %s\n",
                curl_easy_strerror(res));
        
        /* Завершение работы */
        curl_easy_cleanup(curl);
        fclose(fp);
    }

    return 0;
}
```
Пример вывода:
```
(Нет вывода, но проверьте текущую директорию на наличие файла 'downloaded_page.html')
```

## Глубокое погружение
В заре интернета, для получения веб-страницы использовались сырые HTTP-запросы через TCP-сокеты - что было довольно обременительно. В наши дни у нас есть библиотеки, такие как libcurl, которые берут на себя всю рутинную работу. Она обрабатывает все тонкости HTTP-запросов, SSL-соединений и многого другого.

Есть несколько альтернатив libcurl, таких как wget и http-client в C, но libcurl широко используется из-за своей надежности и функциональности. Используя libcurl, помните об этом:

- Инициализация с `curl_easy_init()` обязательна.
- Установите опции, соответствующие вашим потребностям; для скачивания нам нужно указать URL и функцию записи.
- `CURLOPT_WRITEFUNCTION` позволяет нам передать указатель на нашу функцию обратного вызова для записи данных в файл.
- Всегда проверяйте результат `curl_easy_perform()` на наличие ошибок.
- Не забывайте о завершении работы с `curl_easy_cleanup()`, чтобы предотвратить утечки.

Для продакшн-кода вам нужна обработка ошибок, проверка HTTP-статус-кодов и управление вопросами безопасности (например, проверка SSL-сертификатов).

## Смотрите также
- [libcurl](https://curl.se/libcurl/)
- [HTTP Made Really Easy](https://www.jmarshall.com/easy/http/)
