---
title:                "Надсилання HTTP-запиту з базовою автентифікацією"
aliases:
- /uk/cpp/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:01:13.307348-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту з базовою автентифікацією"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що і Навіщо?
Відправка HTTP-запиту з базовою аутентифікацією – це процес, що дозволяє вам безпечно передавати імена користувачів і паролі через HTTP. Програмісти роблять це, щоб забезпечити доступ до захищених ресурсів.

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
