---
title:                "Надсилання http запиту"
html_title:           "C++: Надсилання http запиту"
simple_title:         "Надсилання http запиту"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що & Чому?

Відправлення HTTP-запиту означає відправлення запиту до веб-сервера за допомогою протоколу HTTP. Це один зі способів зв'язку програм зі світом Інтернету. Програмісти виконують це для отримання даних з веб-сервера або для взаємодії з веб-додатками.

## Як:

```C++
// Приклад відправлення HTTP GET запиту за допомогою бібліотеки curl
#include <curl/curl.h>

int main() 
{
  CURL *curl;
  CURLcode res;
  
  // Створення нового об'єкту для виконання запитів
  curl = curl_easy_init();
  if(curl) 
  {
    // Встановлення адреси запиту 
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
    // Встановлення параметрів запиту
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    
    // Виконання запиту та отримання результату
    res = curl_easy_perform(curl);
  }
  
  // Перевірка результату виконання
  if(res != CURLE_OK)
    fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
  
  // Закриття об'єкту
  curl_easy_cleanup(curl);
  
  return 0;
}
```

Вихідний код цього прикладу відправляє GET запит на адресу `https://www.example.com` та виводить на екран отриманий результат.

## Глибше:

HTTP (Hypertext Transfer Protocol) був розроблений для обміну даними між клієнтами та серверами у мережі Інтернет. На сьогоднішній день існують кілька альтернативних способів взаємодії програм з веб-серверами, таких як REST та WebSocket. Однак, HTTP-запити все ще використовуються для багатьох завдань, таких як отримання інформації з веб-сторінок або взаємодії з API веб-додатків. Для відправлення HTTP-запитів, програмістам зазвичай потрібно встановити спеціальні бібліотеки, такі як curl чи HTTP Client.

## Дивись також:

- [Документація з бібліотеки curl](https://curl.haxx.se/libcurl/)
- [Офіційна специфікація протоколу HTTP](https://tools.ietf.org/html/rfc7230)
- [Огляд REST та WebSocket протоколів](https://www.oreilly.com/library/view/developing-restful-web/9781449359737/ch01.html)