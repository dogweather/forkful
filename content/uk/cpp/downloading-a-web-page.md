---
title:                "Завантаження веб-сторінки"
html_title:           "C++: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому

На сьогоднішній день, Інтернет став важливим джерелом інформації для багатьох людей. Завантаження сторінок веб-сайтів стало необхідним процесом для отримання нової інформації, дослідження або навіть для виконання роботи. Це також може бути корисним для розробників, які хочуть отримати дані з Інтернету для подальшого використання в своїх проектах.

## Як це зробити

```C++
#include <iostream>
#include <curl/curl.h> // бібліотека для роботи з HTTP запитами

using namespace std;

// функція для збереження сторінки веб-сайту у файл
static size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream)
{
  size_t written = fwrite(ptr, size, nmemb, stream);
  return written;
}

int main()
{
  // ініціалізація бібліотеки libcurl
  curl_global_init(CURL_GLOBAL_ALL);

  // створення об'єкту типу CURL, який буде виконувати запит
  CURL *curl;
  curl = curl_easy_init();

  // встановлення URL адреси для запиту та файлу для збереження сторінки
  curl_easy_setopt(curl, CURLOPT_URL, "https://uk.wikipedia.org");
  FILE *fp = fopen("uk-wikipedia.html", "wb"); // окремий файл для кожного сайту
  
  // встановлення функції для збереження результату запиту
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);

  // виконання запиту
  curl_easy_perform(curl);

  // закриття файлу та звільнення ресурсів
  fclose(fp);
  curl_easy_cleanup(curl);

  return 0;
}
```

Після виконання цього коду, наша програма завантажить сторінку веб-сайту "https://uk.wikipedia.org" та збереже її у файл з назвою "uk-wikipedia.html". В результаті, ми зможемо переглянути цю сторінку офлайн або використовувати її для подальшої обробки.

## Детальніше про завантаження сторінки веб-сайту

Щоб зрозуміти процес завантаження сторінки веб-сайту, потрібно знати про HTTP протокол. Це протокол, який використовується для обміну даними між сервером та клієнтом. При завантаженні сторінки веб-сайту, програма виконує HTTP запит до сервера, який повертає дані у вигляді HTML коду сторінки. Потім, цей HTML код зберігається у файл для подальшого використання.

Для завантаження сторінки веб-сайту використовується бібліотека libcurl, яка надає функціонал для здійснення HTTP запитів. У нашому прикладі, ми в