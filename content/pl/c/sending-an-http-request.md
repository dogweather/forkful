---
title:                "Wysyłanie żądania HTTP"
aliases:
- pl/c/sending-an-http-request.md
date:                  2024-02-03T18:08:36.313872-07:00
model:                 gpt-4-0125-preview
simple_title:         "Wysyłanie żądania HTTP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/sending-an-http-request.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP polega na tworzeniu i wysyłaniu żądania do serwera WWW w celu pobrania lub przesłania danych. Programiści robią to w języku C, aby współdziałać z interfejsami API stron internetowych, pobierać strony internetowe lub komunikować się z innymi usługami sieciowymi bezpośrednio z ich aplikacji.

## Jak to zrobić:

Aby wysłać żądanie HTTP w języku C, zazwyczaj opierasz się na bibliotekach takich jak libcurl, ponieważ C nie ma wbudowanego wsparcia dla protokołów internetowych. Oto prosty przykład użycia libcurl do wykonania żądania typu GET:

Najpierw upewnij się, że na twoim systemie zainstalowany jest libcurl. Następnie dołącz niezbędne nagłówki i zlinkuj z biblioteką libcurl w twoim pliku źródłowym:

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init(); // Inicjalizacja uchwytu libcurl
    if(curl) {
        // Ustawienie adresu URL, który otrzyma uchwyt libcurl
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        // Zdefiniowanie funkcji zwrotnej do odbierania danych
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL); 
        
        // Wykonanie żądania, res otrzyma kod powrotu
        res = curl_easy_perform(curl);
        // Sprawdzenie błędów
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));

        // Zawsze wykonuj czyszczenie
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Kompiluj to za pomocą czegoś w stylu `gcc -o http_request http_request.c -lcurl`, uruchomienie powinno wykonać proste żądanie GET do "http://example.com".

### Przykładowe wyjście

Ponieważ przykład nie przetwarza odpowiedzi serwera, jego uruchomienie nie wygeneruje widocznych wyników poza potencjalnymi komunikatami o błędach. Integracja funkcji zwrotnej do przetwarzania otrzymanych danych jest kluczowa dla znaczącej interakcji.

## Dogłębna analiza

Koncepcja wysyłania żądań HTTP z programu w języku C opiera się na potężnych możliwościach sieciowych tego języka, w połączeniu z zewnętrznymi bibliotekami, ponieważ sam C jest językiem niskopoziomowym bez wbudowanego wsparcia dla protokołów internetowych. Historycznie, programiści ręcznie używali programowania gniazd w języku C, złożonego i żmudnego procesu, do interakcji z serwerami internetowymi przed pojawieniem się dedykowanych bibliotek takich jak libcurl.

Libcurl, zbudowany na bazie C, usprawnia ten proces, abstrahując od trudnych szczegółów programowania gniazd i specyfiki protokołów HTTP. Wspiera ona wiele protokołów poza HTTP/HTTPS, w tym FTP, SMTP i inne, czyniąc ją wszechstronnym narzędziem do programowania sieciowego w C.

Chociaż używanie libcurl do żądań HTTP w języku C jest praktyczne, współczesne programowanie często skłania się ku językom z wbudowanym wsparciem dla takich zadań, jak Python (biblioteka requests) czy JavaScript (API Fetch). Te alternatywy oferują prostszą, bardziej czytelną składnię kosztem granularnej kontroli i optymalizacji wydajności możliwych w C poprzez bezpośrednią manipulację gniazdami i precyzyjne użycie biblioteki.

Dla krytycznych aplikacji pod względem wydajności lub tam, gdzie konieczna jest bezpośrednia interakcja na poziomie systemu, C pozostaje praktyczną opcją, szczególnie z libcurl ułatwiającym przezwyciężanie złożoności komunikacji internetowej. Jednak dla większości interakcji internetowych na wysokim poziomie, zbadanie bardziej dedykowanych języków programowania internetowego może okazać się bardziej efektywne.
