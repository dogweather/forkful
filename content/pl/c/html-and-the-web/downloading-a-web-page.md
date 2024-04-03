---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:03.879370-07:00
description: "Jak to zrobi\u0107: Aby pobra\u0107 stron\u0119 internetow\u0105 w C,\
  \ jednym z popularnych podej\u015B\u0107 jest u\u017Cycie biblioteki libcurl, czyli\
  \ efektywnej i przeno\u015Bnej biblioteki do\u2026"
lastmod: '2024-03-13T22:44:35.885113-06:00'
model: gpt-4-0125-preview
summary: "Aby pobra\u0107 stron\u0119 internetow\u0105 w C, jednym z popularnych podej\u015B\
  \u0107 jest u\u017Cycie biblioteki libcurl, czyli efektywnej i przeno\u015Bnej biblioteki\
  \ do transferu URL po stronie klienta."
title: Pobieranie strony internetowej
weight: 42
---

## Jak to zrobić:
Aby pobrać stronę internetową w C, jednym z popularnych podejść jest użycie biblioteki libcurl, czyli efektywnej i przenośnej biblioteki do transferu URL po stronie klienta. Upewnij się, że masz zainstalowaną i dołączoną do swojego projektu bibliotekę libcurl. Oto przykład demonstrujący, jak użyć libcurl do pobrania zawartości strony internetowej:

```c
#include <stdio.h>
#include <curl/curl.h>

size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t written = fwrite(ptr, size, nmemb, stream);
    return written;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://example.com";
    char outfilename[FILENAME_MAX] = "./downloaded_page.html";

    curl = curl_easy_init(); // Inicjalizacja sesji libcurl easy
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data); // Ustawienie funkcji zwrotnej do zapisywania otrzymanych danych
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp); // Ustawienie wskaźnika pliku do zapisu danych

        res = curl_easy_perform(curl); // Wykonanie pobierania pliku
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));
        }

        /* zawsze czyść po sobie */
        curl_easy_cleanup(curl); // Oczyszczenie sesji easy
        fclose(fp); // Zamknięcie strumienia pliku
    }
    return 0;
}
```
Przykładowe wyjście (brak widocznego wyjścia w konsoli): Ten kod pobiera zawartość z określonego URL i zapisuje ją do pliku o nazwie `downloaded_page.html`. Sprawdź katalog swojego programu, aby zobaczyć pobraną zawartość.

## Głębsze zanurzenie:
Historycznie, pobieranie zawartości sieciowej w C było bardziej kłopotliwe, wymagało ręcznego programowania gniazd i obsługi protokołu HTTP. Libcurl abstrahuje te złożoności, oferując solidne i wysokopoziomowe API do transferu danych przez sieć.

Chociaż libcurl upraszcza żądania HTTP w C, nowoczesne języki programowania, takie jak Python z jego biblioteką `requests` lub JavaScript (Node.js) z różnymi bibliotekami klienta HTTP, mogą oferować bardziej intuicyjną składnię i wbudowane wsparcie dla JSON i innych formatów danych powszechnie używanych w komunikacji sieciowej. Jednakże, C i libcurl zapewniają wysokowydajne i stabilne rozwiązanie dla systemów, w których istotne są efektywność, kontrola na poziomie detali lub integracja z istniejącymi bazami kodu C. Warto również zauważyć, że C w połączeniu z libcurl może być używane nie tylko do pobierania stron internetowych – jest zdolne do obsługi FTP, SMTP i wielu innych, co czyni je wszechstronnym narzędziem w zestawie programisty.
