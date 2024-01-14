---
title:                "C++: Wysyłanie żądania http z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach wiele aplikacji wymaga połączenia z zewnętrznym serwerem w celu pobrania lub przesyłania danych. Aby to zrobić, często konieczne jest uwierzytelnienie, czyli weryfikacja tożsamości użytkownika. W artykule tym omówimy, dlaczego jest to ważne oraz jak użyć podstawowego uwierzytelnienia HTTP w języku C++.

## Jak to zrobić

Zacznijmy od przykładu kodu, który pokaże nam, jak wysłać żądanie HTTP z uwierzytelnieniem. Korzystać będziemy z biblioteki "curl", w której znajduje się funkcja "curl_easy_perform", która pozwala na wysyłanie zapytania HTTP. Podajemy również parametry URL, metody oraz nazwy użytkownika i hasło w formacie "username:password".

```C++
#include <curl/curl.h>
#include <stdio.h>
#include <iostream>

int main()
{
    CURL* curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();
    if (curl) {
        // Ustawienie URL, metody oraz uwierzytelnienia
        curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
        curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "GET");
        curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");

        // Wysłanie zapytania HTTP
        res = curl_easy_perform(curl);

        // Sprawdzenie statusu odpowiedzi
        if (res != CURLE_OK) {
            // W przypadku błędu wypisze informację o błędzie
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                curl_easy_strerror(res));
        }
        // Zakończenie działania biblioteki
        curl_easy_cleanup(curl);
    }
    // Zakończenie globalnego inicjalizowania biblioteki
    curl_global_cleanup();
}
```

Po wykonaniu powyższego kodu, otrzymamy odpowiedź ze strony "www.example.com", która została wysłana z uwierzytelnieniem dla użytkownika "username" i hasła "password".

## Deep Dive

Użycie podstawowego uwierzytelnienia HTTP w połączeniu z biblioteką "curl" jest jednym z najprostszych sposobów na uwierzytelnienie naszego zapytania do serwera. Funkcja "curl_easy_setopt" pozwala na ustawienie wielu różnych opcji, takich jak adres URL, metoda, czy nawet nagłówki.

W przypadku gdy strona korzysta z innego sposobu uwierzytelniania lub wymaga bardziej szczegółowych danych, należy poszukać odpowiedniej funkcji w bibliotece "curl", która pozwoli na przesłanie danych zgodnie z wymaganiami danej strony.

## Zobacz także

- Dokumentacja biblioteki "curl": https://curl.haxx.se/libcurl/c/
- Przykłady kodów z użyciem biblioteki "curl": https://curl.haxx.se/libcurl/c/example.html
- Tutoriale dotyczące wysyłania żądań HTTP w języku C++: https://www.tutorialspoint.com/curl/curl_http_examples.htm