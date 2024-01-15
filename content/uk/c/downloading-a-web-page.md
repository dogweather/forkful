---
title:                "Завантаження веб-сторінки"
html_title:           "C: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому
Попереднє завантаження веб-сторінки може бути корисним для отримання доступу до інформації, яка відображається в Інтернеті, та використання її в програмах або скриптах.

## Як це зробити
Використовуючи мову програмування C, можна з легкістю завантажити сторінку з Інтернету. Нижче наведено приклад коду для завантаження сторінки та його виконання:

```C
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  FILE *fp;
  CURLcode res;
  char *url = "https://www.example.com/";
  char outfilename[FILENAME_MAX] = "page.html";

  curl = curl_easy_init();
  if (curl)
  {
    fp = fopen(outfilename, "wb");
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
    res = curl_easy_perform(curl);
    curl_easy_cleanup(curl);
    fclose(fp);
  }
  return 0;
}
```

Як результат, цей код завантажує сторінку в HTML-файл з назвою "page.html".

## Глибоке погруження
Іноді, для більш складних завдань, потрібно поглибитись у технічні деталі попереднього завантаження сторінки. Наприклад, ви можете налаштувати CURL, щоб додатково передати параметри запиту або обробити дані, що повертаються з сервера. Для цього варто заглянути до офіційної документації CURL та прочитати про можливі опції та функції.

## Дивіться також
- [Офіційна документація CURL](https://curl.se/docs/)
- [Приклади коду для завантаження веб-сторінок в C](https://www.includehelp.com/c-programs/download-web-page.aspx)
- [Курс "Програмування на мові C" на Codeacademy](https://www.codecademy.com/learn/learn-c)