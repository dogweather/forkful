---
title:                "Надсилаємо HTTP-запит з базової аутентифікацією"
html_title:           "C#: Надсилаємо HTTP-запит з базової аутентифікацією"
simple_title:         "Надсилаємо HTTP-запит з базової аутентифікацією"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Відправлення HTTP-запиту з базовою аутентифікацією в C++ 

## Що це і навіщо це потрібно?
Відправлення HTTP-запиту з базовою аутентифікацією в C++ — це процес, коли ваша програма відправляє запит до веб-сервера, авторизується, використовуючи ім'я користувача та пароль. Це роблять, щоб отримати доступ до приватних ресурсів на сервері.

## Як це робити:
Тут ми використовуємо бібліотеку cURL. Приклад коду C++:
```C++
#include <iostream>
#include <string>
#include <curl/curl.h>

size_t WriteCallback(void* contents, size_t size, size_t nmemb, std::string* userp) {
    userp->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL* curl;
    CURLcode res;
    std::string readBuffer;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://mywebsite.com/protected-resource");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERNAME, "myusername");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "mypassword");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);

        res = curl_easy_perform(curl);

        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

        curl_easy_cleanup(curl);
    }

    curl_global_cleanup();

    return 0;
}
```
## Пірнання вглиб: 
HTTP Basic Authentication був впроваджений ще в 1996 році в якості частини специфікації HTTP/1.0. Хоча він і дуже простий в використанні, він не надає високого рівня безпеки, оскільки ім'я користувача та пароль передаються в нешифрованому вигляді. 

Альтернативами є більш сучасні методи аутентифікації, такі як OAuth або JWT. 

Враховуючи деталі реалізації, важливо згадати, що коректне використання бібліотеки cURL потребує правильної ініціалізації та очищення.

## Цікаві посилання:  
1. [Документація cURL](https://curl.haxx.se/libcurl/c/)
2. [Розділ про аутентифікацію в MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
3. [HTTP/1.0 специфікації](https://www.w3.org/Protocols/HTTP/1.0/spec.html#BasicAA)