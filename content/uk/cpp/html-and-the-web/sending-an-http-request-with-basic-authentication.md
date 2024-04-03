---
date: 2024-01-20 18:01:13.307348-07:00
description: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E\
  \ \u2013 \u0446\u0435 \u043F\u0440\u043E\u0446\u0435\u0441, \u0449\u043E \u0434\u043E\
  \u0437\u0432\u043E\u043B\u044F\u0454 \u0432\u0430\u043C \u0431\u0435\u0437\u043F\
  \u0435\u0447\u043D\u043E \u043F\u0435\u0440\u0435\u0434\u0430\u0432\u0430\u0442\u0438\
  \ \u0456\u043C\u0435\u043D\u0430 \u043A\u043E\u0440\u0438\u0441\u0442\u0443\u0432\
  \u0430\u0447\u0456\u0432 \u0456 \u043F\u0430\u0440\u043E\u043B\u0456 \u0447\u0435\
  \u0440\u0435\u0437 HTTP. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\
  \u0438\u2026"
lastmod: '2024-03-13T22:44:49.842942-06:00'
model: gpt-4-1106-preview
summary: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E\
  \ \u2013 \u0446\u0435 \u043F\u0440\u043E\u0446\u0435\u0441, \u0449\u043E \u0434\u043E\
  \u0437\u0432\u043E\u043B\u044F\u0454 \u0432\u0430\u043C \u0431\u0435\u0437\u043F\
  \u0435\u0447\u043D\u043E \u043F\u0435\u0440\u0435\u0434\u0430\u0432\u0430\u0442\u0438\
  \ \u0456\u043C\u0435\u043D\u0430 \u043A\u043E\u0440\u0438\u0441\u0442\u0443\u0432\
  \u0430\u0447\u0456\u0432 \u0456 \u043F\u0430\u0440\u043E\u043B\u0456 \u0447\u0435\
  \u0440\u0435\u0437 HTTP."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E"
weight: 45
---

## Як це зробити:
Використовуємо бібліотеку cURL для C++, що дозволяє легко відправляти HTTP-запити. Наведемо приклад коду:

```C++
#include <iostream>
#include <curl/curl.h>
#include <string>

int main() {
    CURL *curl;
    CURLcode res;
    std::string readBuffer;

    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();
    
    if(curl) {
        std::string userPwd = "user:password"; // Замініть на свої логін і пароль
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com"); // URL, до якого ви хочете отримати доступ
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERPWD, userPwd.c_str());
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        res = curl_easy_perform(curl);
        
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        } else {
            std::cout << readBuffer << std::endl;
        }

        curl_easy_cleanup(curl);
    }
    curl_global_cleanup();

    return 0;
}
```

Ця програма виведе вміст веб-сторінки або помилку, якщо запит не вдасться.

## Глибше занурення:
Базова аутентифікація – це старий, але простий спосіб захисту HTTP-запитів. Використовується набір імені користувача й пароля, закодований у форматі Base64, але це не найбезпечніший метод. 

Альтернативи включають OAuth, tokens, або Digest Authentication, які забезпечують більш сильний захист. При використанні базової аутентифікації завжди переконуйтесь, що ваші запити здійснюються через HTTPS, а не HTTP.

Коли ви використовуєте бібліотеку cURL у C++, ви можете легко включити аутентифікаційні дані з використанням опції CURLOPT_USERPWD і передати їх разом з вашим запитом. cURL догрижається за вас про кодування ваших облікових даних в Base64.

## Додатково:
- [libcurl](https://curl.se/libcurl/) – офіційний сайт бібліотеки cURL.
- [HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication) – документація MDN Web Docs про аутентифікацію HTTP.
- [HTTP Authentication: Basic and Digest Access Authentication (RFC 7617)](https://tools.ietf.org/html/rfc7617) – опис базової аутентифікації від IETF.
