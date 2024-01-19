---
title:                "Надсилаємо HTTP-запит з базової аутентифікацією"
html_title:           "C#: Надсилаємо HTTP-запит з базової аутентифікацією"
simple_title:         "Надсилаємо HTTP-запит з базової аутентифікацією"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що і навіщо?
Відправлення HTTP-запиту з базовою автентифікацією - це процес передачі ваших облікових данних серверу для перевірки. Програмісти роблять це, щоб забезпечити безпечність даних та доступ до особистих ресурсів.

## Як це зробити:
```C
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERPWD, "user:password");

        res = curl_easy_perform(curl);
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));

        curl_easy_cleanup(curl);
    }

    curl_global_cleanup();

    return 0;
}
```
Вищенаведений код відправить HTTP-запит до "http://example.com" з базовою автентифікацією, використовуючи облікові дані "user:password".

## Докладний погляд
Відправлення HTTP-запитів із базовою автентифікацією було впроваджено в RFC 2617 у 1999 році як частина протоколу HTTP 1.1.

Щодо альтернатив, ви можете використовувати дайджест-автентифікацію, токени OAuth або SSL-сертифікати для автентифікації на сервері.

Існує декілька різних бібліотек для відправлення HTTP-запитів у C, але libcurl є однією з найпопулярніших частин, частково через свою підтримку різних методів автентифікації.

## Дивіться також
[Офіційна документація libcurl](https://curl.se/libcurl/c/)

[HTTP-автентифікація на MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)

[RFC 2617 - HTTP Authentication: Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc2617)