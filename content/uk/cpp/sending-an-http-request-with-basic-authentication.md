---
title:                "Надсилання http запиту з базовою аутентифікацією"
html_title:           "C++: Надсилання http запиту з базовою аутентифікацією"
simple_title:         "Надсилання http запиту з базовою аутентифікацією"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

Відправка запиту HTTP з базовою аутентифікацією є необхідною для доступу до захищених ресурсів, які вимагають авторизації. Це забезпечує безпеку передачі даних та забезпечує, що тільки визначені користувачі мають доступ до захищених ресурсів.

## Як

```C++
// Приклад коду для відправки HTTP запиту з базовою аутентифікацією
#include <iostream>
#include <curl/curl.h>

int main()
{
    // Ініціалізація CURL змінної
    CURL *curl;
    curl = curl_easy_init();

    // Вказівка URL
    curl_easy_setopt(curl, CURLOPT_URL, "https://example.com/protected_resource");

    // Додавання заголовку для базової аутентифікації
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, "Authorization: Basic <base64 encoded credentials>");

    // Надіслати запит
    CURLcode res = curl_easy_perform(curl);

    // Перевірка успішності відправки та виведення результату
    if (res == CURLE_OK)
    {
        std::cout << "Запит успішно відправлено";
    }
    else
    {
        std::cout << "Помилка у відправці запиту: " << curl_easy_strerror(res);
    }

    // Закриття CURL змінної
    curl_easy_cleanup(curl);

    return 0;
}
```
Вивід:
```
Запит успішно відправлено.
```

## Глибокий Занурення

HTTP базова аутентифікація використовує заголовок "Authorization", який містить ідентифікатор користувача та пароль, закодований у форматі Base64. За замовчуванням браузери видаляють авторизаційні дані з цього заголовка, тому що він не є надійним механізмом безпеки. До того ж, базова аутентифікація не зашифровує дані при передачі, тому є потенційно небезпечною для важливої інформації.

## Дивись також

- [HTTP Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [C++ Libcurl](https://curl.se/libcurl/c/CURLOPT_HTTPHEADER.html)