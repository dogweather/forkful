---
title:                "Надсилання HTTP-запиту з базовою автентифікацією"
date:                  2024-01-20T18:01:37.054611-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту з базовою автентифікацією"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що це та Навіщо?
Отже, HTTP-запити з базовою автентифікацією — це коли ви відправляєте ім'я користувача та пароль у заголовку запиту, щоб отримати доступ до захищеного ресурсу. Програмісти роблять це, щоб взаємодіяти з веб-сервісами, які вимагають перевірки автентичності.

## Як це зробити:
```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init();
    if(curl) {
        // Встановлення URL
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/resource");
        
        // Встановлення заголовку для базової автентифікації
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERNAME, "user");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "pass");
        
        // Виконання запиту
        res = curl_easy_perform(curl);
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        
        // Завершення сесії
        curl_easy_cleanup(curl);
    }
    return 0;
}
```
Вище наведений код демонструє, як виконати запит з базовою автентифікацією за допомогою бібліотеки cURL. Якщо все пройшло успішно, відповідь сервера буде виведена на стандартний вихід. У разі помилки, ви побачите опис помилки.

## Поглиблені знання
Базова автентифікація — це метод автентифікації HTTP, в якому логін і пароль (не зашифровані) включаються в заголовок запиту. Це старий і простий, але не найбезпечніший спосіб передачі облікових даних. Як альтернативу, розгляньте OAuth або токени, які забезпечують більш безпечну авторизацію. Щодо реалізації у мові C, бібліотека cURL є популярним вибором для роботи з HTTP-запитами, оскільки вона підтримує широкий спектр протоколів і характеризується гнучкістю та простотою у використанні.

## Дивіться також:
- [cURL libcurl - Using libcurl in C programs](https://curl.se/libcurl/c/)
- [RFC 7617 – The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [RFC 6750 – The OAuth 2.0 Authorization Framework: Bearer Token Usage](https://tools.ietf.org/html/rfc6750)