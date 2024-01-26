---
title:                "Wysyłanie zapytania http z podstawową autoryzacją"
date:                  2024-01-20T18:00:55.093083-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie zapytania http z podstawową autoryzacją"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wysyłanie żądania HTTP z podstawową autoryzacją to proces, w którym klient przesyła swoje dane uwierzytelniające do serwera, aby uzyskać dostęp do zabezpieczonych zasobów. Programiści używają tego, gdy potrzebują bezpiecznego sposobu na potwierdzenie tożsamości użytkownika przez aplikację sieciową.

## Jak to zrobić:
```C
#include <stdio.h>
#include <curl/curl.h>

int main() {
    CURL *curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERNAME, "user");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "password");

        CURLcode res = curl_easy_perform(curl);
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

        curl_easy_cleanup(curl);
    }

    return 0;
}
```
Powyższy przykład pokazuje, jak wysłać proste żądanie typu GET z użyciem autoryzacji podstawowej z pomocą biblioteki libcurl w C.

## W głębi tematu
Początkowo HTTP Basic Authentication było podstawowym sposobem uwierzytelniania w sieci. Mimo że obecnie istnieją bardziej zaawansowane metody, takie jak OAuth czy JWT, metoda podstawowa wciąż znajduje zastosowanie, np. w internalnych API czy prostych aplikacjach webowych.

Alternatywy jak tokeny Bearer wymagają pewnej formy tokena, który jest zdobywany zewnętrznie, co czyni proces bardziej złożonym.

Podstawowa uwierzytalność http to base64 zakodowana wartość 'użytkownik:hasło', którą należy wysłać w nagłówku żądania. Pamiętaj, że base64 nie jest metodą szyfrowania i łatwo można odkodować te wartości. Dlatego zawsze używaj HTTPS, by chronić dane uwierzytelniające w transmisji.

## Zobacz również:
- Dokumentacja libcurl: https://curl.se/libcurl/
- Specyfikacja HTTP Basic Authentication: https://tools.ietf.org/html/rfc7617
- Przewodnik po uwierzytelnianiu w http: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Wprowadzenie do bezpiecznych tokenów i JWT: https://jwt.io/introduction/
