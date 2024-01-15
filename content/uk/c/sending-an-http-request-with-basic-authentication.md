---
title:                "Надсилання http запиту з основною автентифікацією."
html_title:           "C: Надсилання http запиту з основною автентифікацією."
simple_title:         "Надсилання http запиту з основною автентифікацією."
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

Чому: Базова автентифікація є одним з найпростіших методів автентифікації для HTTP запитів. Вона дозволяє забезпечити безпеку під час передачі даних між клієнтом та сервером за допомогою базового рівня безпеки.

## How To

Для надсилання HTTP запиту з базовою автентифікацією у C, потрібно встановити заголовок "Authorization" зі значенням "Basic", за яким потрібно додати закодоване користувачем ім'я та пароль. Нижче наведений кодовий приклад:

```C
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<curl/curl.h>

int main(void)
{
    CURL *curl;
    CURLcode res;
    
    // Оголошення змінних для користувача та пароля
    char *user = "username";
    char *pass = "password";
    
    // Конвертування користувача та пароля до потрібного формату для базової автентифікації
    char auth[255];
    sprintf(auth, "%s:%s", user, pass);
    
    // Кодування користувача та пароля за допомогою base64
    char encoded[255];
    snprintf(encoded, sizeof(encoded), "Basic %s", (char*)base64_encode((const unsigned char*)auth, strlen(auth)));
    
    // Ініціалізація CURL
    curl = curl_easy_init();
    if (curl) {
        // Встановлення URL
        curl_easy_setopt(curl, CURLOPT_URL, "http://www.example.com");
        
        // Додавання заголовку автентифікації
        struct curl_slist *headers = NULL;
        headers = curl_slist_append(headers, encoded);
        curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
        
        // Виконання запиту
        res = curl_easy_perform(curl);
        
        // Перевірка на помилки та закриття CURL
        if (res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
            curl_easy_strerror(res));
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Результат виконання програми буде виглядати наступним чином:

```
HTTP/1.1 200 OK
Date: Mon, 07 Jun 2021 00:00:00 GMT
Server: Apache
Content-Length: 123
Content-Type: text/plain; charset=UTF-8

Hello, world!
```

## Deep Dive

Для проведення базової автентифікації у C потрібно знати, як конвертувати користувача та пароль до потрібного формату, а також як кодувати їх за допомогою base64. Також слід знати, що базова автентифікація не є найбезпечнішим методом автентифікації, тому рекомендується використовувати її тільки для простих запитів та зв'язку з безпечними джерелами.

## See Also

Перевірте наступні посилання для додаткової інформації про базову автентифікацію у C:

- [Документація CURL](https://curl.haxx.se/libcurl/c/CURLOPT_HTTPHEADER.html)
- [Розширена інформація про