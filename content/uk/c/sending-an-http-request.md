---
title:                "Відправлення запиту http"
html_title:           "C: Відправлення запиту http"
simple_title:         "Відправлення запиту http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому
Перед тим, як перейти до розгляду засобів створення HTTP запитів, важливо зрозуміти, що це дає користувачеві. Використання HTTP запитів дозволяє взаємодіяти з веб-сайтами, отримуючи необхідну інформацію чи виконуючи певні дії.

## Як
Для створення HTTP запиту у С потрібно використовувати певні функції та бібліотеки. Розглянемо приклад коду для створення GET запиту:

```C
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://example.com");
    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    curl_easy_cleanup(curl);
  }
  return 0;
}
```
Цей код створює CURL з'єднання з вказаною URL-адресою та виконує GET запит до сервера. Результатом виконання запиту буде HTML сторінка відповідної веб-сторінки.

## Глибоке пірнання
HTTP запити можуть бути виконані за допомогою різних методів: GET, POST, PUT, DELETE. Крім того, можна передавати параметри та заголовки в запиті. Для цього можна скористатися різними функціями та параметрами CURL. Детальнішу інформацію про це можна знайти у документації.

## Дивіться також
- [Офіційна документація з CURL](https://curl.se/libcurl/c/)
- [Стаття про роботу з HTTP запитами в С](https://codeforces.com/blog/entry/49395)
- [Стаття про використання CURL для HTTP запитів](https://www.geeksforgeeks.org/how-to-send-a-get-request-using-curl-2/)