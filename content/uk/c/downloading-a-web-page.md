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

## Що & чому?

Завантаження веб-сторінки - це коли ваша програма отримує веб-сторінку з Інтернету і зберігає її на вашому комп'ютері. Програмісти роблять це, щоб мати доступ до вмісту веб-сторінки для подальшої обробки або використання у своїх програмах.

## Як:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com/");
    res = curl_easy_perform(curl);
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    curl_easy_cleanup(curl);
  }
  return 0;
}
```
Вивід:
```
<!doctype html>
<html>
<head>
  <title>Example Domain</title>

  <meta charset="utf-8" />
  <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <style type="text/css">
  body {
    background-color: #f0f0f2;
    margin: 0;
    padding: 0;
    font-family: -apple-system, system-ui, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
  }
  div {
    width: 600px;
    margin: 5em auto;
    padding: 2em;
    background-color: #fdfdff;
    border-radius: 0.5em;
    box-shadow: 2px 3px 7px 2px rgba(0,0,0,0.02);
  }
  a:link, a:visited {
    color: #38488f;
    text-decoration: none;
  }
  @media (max-width: 700px) {
    body {
      background-color: #fff;
    }
    div {
      width: auto;
      margin: 0 auto;
      border-radius: 0;
      padding: 1em;
      box-shadow: none;
    }
  }
  </style>
</head>

<body>
<div>
  <h1>Example Domain</h1>
  <p>This domain is for use in illustrative examples in documents. You may use this
  domain in literature without prior coordination or asking for permission.</p>
  <p><a href="https://www.iana.org/domains/example">More information...</a></p>
</div>
</body>
</html>
```

## Розглянути детальніше:

**Історичний контекст:** Завантаження веб-сторінки є однією з основних дій, які виконують програми з Інтернетом. Це стало можливим завдяки впровадженню протоколу HTTP у 1990-х роках, який дозволяє обмінюватись даними між веб-сервером і веб-клієнтом. З тих пір завантаження веб-сторінки стало необхідною частиною розробки веб-додатків.

**Альтернативи:** Існує багато інших способів завантаження веб-сторінок, таких як використання інших протоколів, наприклад FTP або UDP, або використання готових бібліотек, наприклад libcurl. Однак, завантаження веб-сторінки за допомогою протоколу HTTP є найпоширенішим підходом.

**Деталі реалізації:** У прикладі вище використовується бібліотека libcurl для здійснення HTTP-запиту на веб-сторінку. За допомогою функцій, таких як `curl_easy_setopt` і `curl_easy_perform`, програміст може встановити параметри запиту та отримати відповідь, включаючи HTML-код сторінки. Для написання більш складних програм, наприклад, парсерів веб-сторінок або ботів, може бути необхідно складніший код.

## Дивіться також:

- [Документація libcurl](https://curl.se/libcurl/)
- [Програмування з використанням HTTP](https://developer.mozilla.org/uk/docs/Learn/Server-side/)