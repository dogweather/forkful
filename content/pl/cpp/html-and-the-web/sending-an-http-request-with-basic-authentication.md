---
date: 2024-01-20 18:01:23.209982-07:00
description: "Wysy\u0142anie \u017C\u0105dania HTTP z podstawow\u0105 autentykacj\u0105\
  \ to proces, w kt\xF3rym klient internetowy przekazuje dane logowania do serwera\
  \ w zabezpieczonym formacie.\u2026"
lastmod: '2024-02-25T18:49:34.084490-07:00'
model: gpt-4-1106-preview
summary: "Wysy\u0142anie \u017C\u0105dania HTTP z podstawow\u0105 autentykacj\u0105\
  \ to proces, w kt\xF3rym klient internetowy przekazuje dane logowania do serwera\
  \ w zabezpieczonym formacie.\u2026"
title: "Wysy\u0142anie zapytania http z podstawow\u0105 autoryzacj\u0105"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Wysyłanie żądania HTTP z podstawową autentykacją to proces, w którym klient internetowy przekazuje dane logowania do serwera w zabezpieczonym formacie. Programiści używają tego, by umożliwić aplikacjom dostęp do zasobów wymagających weryfikacji użytkownika.

## How To (Jak To Zrobić):
C++ nie ma wbudowanego wsparcia dla HTTP, więc użyjemy biblioteki `CURL` oraz dodatkowo `cpp-base64` do zakodowania poświadczeń. Zainstaluj CURL i dołącz go do projektu. Poniżej kod:

```C++
#include <iostream>
#include <curl/curl.h>
#include <string>
#include "base64.h"

int main() {
    CURL *curl = curl_easy_init();

    if(curl) {
        // Twoje dane logowania
        std::string userName = "user";
        std::string password = "pass";
        
        // Zakoduj poświadczenia do Base64
        std::string credentials = base64_encode(userName + ":" + password);
        
        // Ustaw nagłówek autoryzacji
        struct curl_slist *headers = nullptr;
        headers = curl_slist_append(headers, ("Authorization: Basic " + credentials).c_str());

        // Ustaw URL oraz nagłówek
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/resource");
        curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);

        // Wykonaj żądanie
        CURLcode res = curl_easy_perform(curl);
        
        // Sprawdź błędy
        if(res != CURLE_OK)
            std::cerr << "CURL error: " << curl_easy_strerror(res) << std::endl;

        // Posprzątaj
        curl_slist_free_all(headers);
        curl_easy_cleanup(curl);
    }

    return 0;
}
```

Wyjście zależy od odpowiedzi serwera, więc nie podajemy przykładu. Kod powyżej wyśle żądanie GET do `http://example.com/resource`.

## Deep Dive (Dogłębna Analiza):
Kiedyś proste żądania HTTP mogły być wysyłane przez gniazda sieciowe (`sockets`), ale zwiększenie bezpieczeństwa sieci wymusiło użycie silniejszych bibliotek. Najpopularniejsze alternatywy dla CURL to: `Boost.Beast`, `cpp-httplib`, i `Poco`. Podstawowa autentykacja, choć prosta, nie jest najbezpieczniejszą metodą; bardziej zaawansowaną i bezpieczną metodą jest OAuth. Realizacja a klienckiej strony może być mniej skomplikowana niż na serwerowej; klient musi tylko odpowiednio przygotować i wysłać poświadczenia.

## See Also (Zobacz Również):
- Dokumentacja CURL: https://curl.haxx.se/libcurl/c/
- Biblioteka cpp-base64: https://github.com/ReneNyffenegger/cpp-base64
- Boost.Beast: https://www.boost.org/doc/libs/1_75_0/libs/beast/doc/html/index.html
- cpp-httplib: https://github.com/yhirose/cpp-httplib
- Poco Libraries: https://pocoproject.org/
- OAuth: https://oauth.net/
