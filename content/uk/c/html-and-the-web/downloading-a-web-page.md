---
title:                "Завантаження веб-сторінки"
aliases:
- /uk/c/downloading-a-web-page.md
date:                  2024-02-03T17:56:27.531379-07:00
model:                 gpt-4-0125-preview
simple_title:         "Завантаження веб-сторінки"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/downloading-a-web-page.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?

Завантаження веб-сторінки в C передбачає програмний доступ до вмісту веб-сторінки через інтернет та її збереження локально для обробки або використання офлайн. Програмісти часто займаються цим, щоб споживати веб-сервіси, скрейпити веб-контент або взаємодіяти з онлайн ресурсами безпосередньо зі своїх додатків.

## Як:

Для завантаження веб-сторінки в C один із популярних підходів - використання бібліотеки libcurl, ефективної та переносимої клієнтської бібліотеки передачі URL. Переконайтеся, що у вашому проєкті встановлено та підключено libcurl. Ось приклад, що демонструє, як використовувати libcurl для завантаження вмісту веб-сторінки:

```c
#include <stdio.h>
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
    char outfilename[FILENAME_MAX] = "./downloaded_page.html";

    curl = curl_easy_init(); // Ініціалізація простої сесії libcurl
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data); // Зворотній виклик для запису отриманих даних
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp); // Встановлення вказівника файлу для запису даних

        res = curl_easy_perform(curl); // Виконання завантаження файлу
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));
        }

        /* завжди очищайте */
        curl_easy_cleanup(curl); // Очищення простої сесії
        fclose(fp); // Закриття потоку файлу
    }
    return 0;
}
```
Приклад виводу (немає видимого виводу в консоль): Цей код завантажує вміст на вказаному URL і зберігає його у файлі під назвою `downloaded_page.html`. Перевірте директорію вашої програми для цього файлу, щоб побачити завантажений вміст.

## Поглиблений аналіз:

Історично, завантаження веб-контенту в C було більш обтяжливим, вимагаючи ручного програмування сокетів і обробки протоколу HTTP. Libcurl абстрагує ці складнощі, пропонуючи міцний і високорівневий API для передачі даних через веб.

Хоча libcurl спрощує HTTP-запити в C, сучасні мови програмування, як-от Python з його бібліотекою `requests` або JavaScript (Node.js) з різними бібліотеками клієнтів HTTP, можуть пропонувати більш інтуїтивний синтаксис та вбудовану підтримку для JSON та інших форматів даних, які часто використовуються у веб-комунікаціях. Проте, C і libcurl забезпечують високопродуктивне і стабільне рішення для систем, де важливі ефективність, детальний контроль або інтеграція в існуючі бази коду C. Також варто зазначити, що C у поєднанні з libcurl може використовуватися не лише для завантаження веб-сторінок - він здатний на більше, включаючи FTP, SMTP та багато іншого, роблячи його універсальним інструментом у наборі програміста.
