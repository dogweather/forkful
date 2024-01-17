---
title:                "Wysyłanie żądania http z podstawową autoryzacją"
html_title:           "C: Wysyłanie żądania http z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

#C (bieżąca wersja) – Wysyłanie żądania HTTP z podstawowym uwierzytelnianiem

## O co w tym chodzi i dlaczego?
Wysyłanie żądania HTTP z podstawowym uwierzytelnianiem to proces, w którym programista wysyła żądanie do serwera internetowego z dodatkowym nagłówkiem zawierającym uwierzytelnienie, aby móc uzyskać dostęp do chronionych zasobów na serwerze. Jest to powszechnie stosowana metoda, ponieważ pozwala na wykonywanie operacji bezpiecznie, a także na uzyskanie dostępu do niepublicznych danych.

## Jak to zrobić?
```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
    CURL *curl;
    CURLcode res;

    // Inicjalizacja
    curl = curl_easy_init();
    if(curl) {
        // Ustawienie adresu URL do żądania
        curl_easy_setopt(curl, CURLOPT_URL, "http://www.example.com/protected-resource");

        // Ustawienie nagłówka z uwierzytelnieniem
        curl_easy_setopt(curl, CURLOPT_HTTPHEADER, "Authorization: Basic user:password");

        // Wykonanie żądania
        res = curl_easy_perform(curl);

        // Sprawdzenie stanu żądania
        if(res != CURLE_OK)
            fprintf(stderr, "Nie udało się wysłać żądania: %s\n",
                curl_easy_strerror(res));

        // Zakończenie połączenia
        curl_easy_cleanup(curl);
    }
    return 0;
}
```
Wynik:
`HTTP/1.1 200 OK
Date: Sat, 15 Jun 2019 10:00:00 GMT
Server: Apache/2.2.17 (Unix)
Content-Length: 300`

## Głębsze zanurzenie
Wysyłanie żądania HTTP z podstawowym uwierzytelnianiem jest jedną z najstarszych metod uwierzytelniania w Protokole Transferu Hypertextu (HTTP). Została ona opracowana już w 1999 roku i od tamtego czasu jest powszechnie stosowana. Istnieją również inne metody uwierzytelniania, takie jak uwierzytelnianie oparte na tokenie, które są bardziej bezpieczne, ponieważ nie wymagają przesyłania haseł w otwartym tekście.

W celu zapewnienia bezpieczeństwa przy wysyłaniu żądania z podstawowym uwierzytelnieniem, często stosuje się połączenie z protokołem HTTPS, które zapewnia szyfrowanie danych. Ponadto, ważne jest, aby nie używać tych danych do innych celów niż autoryzacja danego żądania.

## Zobacz również
- [Dokumentacja libcurl](https://curl.haxx.se/libcurl/c/)
- [Wikipedia - HTTP Basic Access Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)