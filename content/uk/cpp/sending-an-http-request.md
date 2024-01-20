---
title:                "Надсилання http-запиту"
html_title:           "Arduino: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що та Навіщо?
Надсилання HTTP-запиту - це процес взаємодії програми з веб-сервером для отримання або відправлення даних через протокол HTTP. Програмісти роблять це для взаємодії з API, структурованої взаємодії з веб-сторінками, або для отримання даних з веб-ресурсів.

## Як зробити:
Використовуючи бібліотеку cURL в C++. Переконайтесь, що встановили cURL на вашу систему і підключили його до свого проекту.

```C++
#include <curl/curl.h>

int main() {
  CURL *curl;
  CURLcode res;
  
  curl_global_init(CURL_GLOBAL_DEFAULT);
  curl = curl_easy_init();

  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
 
    res = curl_easy_perform(curl);
    
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
 
    curl_easy_cleanup(curl);
  }
  
  curl_global_cleanup();
  
  return 0;
}
```
Цей код відправляє GET HTTP-запит до example.com.

## Поглиблений матеріал:
Надсилання HTTP-запитів з'явилося майже одночасно з виникненням всесвітньої мережі. Раніше було не так багато альтернатив, але зараз, крім cURL, є багато інших бібліотек, таких як Boost.Asio, POCO і Qt Network. Велика частина цих бібліотек задіяна в асинхронному вводі/виводі і наданні більш високого рівня абстракції.

## Дивіться також:
1. [cURL офіційний сайт](https://curl.haxx.se)
2. [Boost.Asio](https://www.boost.org/doc/libs/1_65_1/doc/html/boost_asio.html)
3. [POCO Net](https://pocoproject.org/docs/Poco.Net.html)
4. [Qt Network](https://doc.qt.io/qt-5/qtnetwork-index.html)