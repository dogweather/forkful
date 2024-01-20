---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pobieranie strony internetowej polega na uzyskaniu dostępu do jej kodu HTML na swoim komputerze. Programiści robią to, aby analizować strukturę strony, co może pomóc w tworzeniu botów, skryptów bądź w pozyskiwaniu danych.

## Jak to zrobić?
Użytkownik musi połączyć się z serwerem, na którym znajduje się strona, a następnie wysłać żądanie HTTP GET. W kodzie C możemy to zrobić za pomocą biblioteki libcurl. Poniżej znajduje się przykład kodu, który pobiera źródło strony www:

```C
#include <curl/curl.h>

int main(void)
{
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

## Do głębi
Historia pobierania stron web sięga powstania protokołu HTTP w 1989 roku. Z biegiem lat rozwinęło się wiele alternatyw, takich jak Scrapy dla Pythona czy Jsoup dla Java. Wybór zależy od języka, w którym programujesz, ale w C libcurl jest standardem.

Detale implementacji mogą się różnić, ale zasada jest taka sama - wysyłka żądania GET do serwera i odbieranie odpowiedzi, która zawiera źródło strony. Istotne jest również właściwe zarządzanie zasobami - chociażby poprzez stosowanie funkcji `curl_easy_cleanup`.

## Zobacz też
- [Dokumentacja libcurl](https://curl.haxx.se/libcurl/c/)
- [Poradnik "How to download a webpage using C"](https://www.example.com/download-webpage)
- [Artykuł "Anatomy of an HTTP Transaction"](https://developer.mozilla.org/pl/docs/Web/HTTP)
- [Poradnik "Scraping a web page in C"](https://progur.com/2016/09/how-to-scrape-web-pages-in-c.html)