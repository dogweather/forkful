---
title:                "C: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach większość aplikacji internetowych korzysta z komunikacji z zasobami zewnętrznymi, takimi jak bazy danych, serwery plików lub API. Aby to osiągnąć, musimy wykorzystać mechanizm znanym jako protokół HTTP (Hypertext Transfer Protocol). Dzięki temu możemy łatwo wysyłać i odbierać dane z różnych źródeł.

## Jak to zrobić

Poniżej przedstawiamy przykładowy kod w języku C, który wyświetla dane odpowiedzi z serwera po wysłaniu żądania HTTP GET. Kod ten wykorzystuje bibliotekę libcurl, która jest dostępna w większości systemów operacyjnych.

```
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
    // Inicjalizacja libcurl
    CURL *curl;
    CURLcode res;
    
    // Przygotowanie adresu URL
    char *url = "https://example.com";

    // Inicjalizacja i konfiguracja sesji curl
    curl = curl_easy_init();
    if(curl) {
        // Ustawianie URL
        curl_easy_setopt(curl, CURLOPT_URL, url);
        // Ustawianie opcji FOLLOWLOCATION w przypadku przekierowania
        curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
        
        // Wysyłanie żądania GET i odbieranie odpowiedzi
        res = curl_easy_perform(curl);
        
        // Sprawdzanie czy wystąpił błąd
        if(res != CURLE_OK)
          fprintf(stderr, "Błąd: %s\n",
                  curl_easy_strerror(res));
    
        // Zamykanie curl
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Po uruchomieniu powyższego kodu, zobaczymy w konsoli wyświetloną zawartość strony https://example.com. Oczywiście możemy zmienić adres URL na inny i wysłać odpowiednią metodą (np. POST) oraz dodawać nagłówki lub dane do żądania.

## Deep Dive

Wysyłanie żądania HTTP za pomocą biblioteki libcurl odbywa się poprzez wywołanie funkcji `curl_easy_perform()`. W odpowiedzi otrzymujemy kod stanu (ang. status code), który informuje nas o wyniku wysyłanego żądania. Na przykład:

- `200` - żądanie zostało pomyślnie przetworzone
- `301` - przeniesiono zasób na stałe
- `404` - żądany zasób nie został znaleziony
- `500` - wewnętrzny błąd serwera

Dodatkowo, w przypadku żądań POST, musimy ustawić daną do wysłania i jej rozmiar za pomocą funkcji `curl_easy_setopt()`. Warto również wspomnieć o funkcjach, które pozwalają nam ustawiać nagłówki żądania i odbierać dane pobrane z odpowiedzi serwera.

Mimo że w tym przykładzie korzystamy z biblioteki libcurl, istnieją również inne sposoby na wysyłanie żądań HTTP w języku C, takie jak biblioteka cURLpp czy funkcje dostępne w systemie operacyjnym.

## Zobacz również

- Oficjalna strona libcurl: https://curl.haxx.se/libcurl/
- Poradnik do biblioteki libcurl (ang.): https://ec.haxx.se/libcurl-programming.html