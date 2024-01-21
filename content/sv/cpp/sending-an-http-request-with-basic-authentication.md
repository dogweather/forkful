---
title:                "Skicka en HTTP-förfrågan med Basic-autentisering"
date:                  2024-01-20T18:01:38.065883-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en HTTP-förfrågan med Basic-autentisering"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-förfrågan med basic-autentisering innebär att du tillhandahåller användarnamn och lösenord för att få åtkomst till en resurs på webben. Programmerare gör detta för att säkra kommunikationen med webbtjänster som kräver inloggning.

## Hur man gör:
```cpp
#include <iostream>
#include <curl/curl.h>
#include <base64.h> // Fiktivt bibliotek för exempel.

int main() {
    CURL *curl = curl_easy_init();
    if(curl) {
        // Bas64-kodar användarnamn och lösenord.
        std::string credentials = base64_encode("användarnamn:lösenord");

        struct curl_slist *headers = NULL;
        headers = curl_slist_append(headers, ("Authorization: Basic " + credentials).c_str());

        curl_easy_setopt(curl, CURLOPT_URL, "http://exempel.se/resurs");
        curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
        
        // Skickar förfrågan och hanterar svar.
        CURLcode res = curl_easy_perform(curl);
        if (res != CURLE_OK) {
            std::cerr << "CURL-fel: " << curl_easy_strerror(res) << std::endl;
        }

        // Städar upp.
        curl_slist_free_all(headers);
        curl_easy_cleanup(curl);
    }
    return 0;
}
```
Sample Output (Kan variera beroende på serverns svar):
```
<respons-innehåll från servern>
```

## Djupdykning:
Att autentisera HTTP-förfrågningar med 'Basic'-metoden har använts sedan HTTP/1.0. Metoden är enkel men inte den säkraste - användarnamn och lösenord kodas i base64 utan kryptering, vilket gör dem sårbart vid nätverksavlyssning. Alternativ inkluderar OAuth, tokens och mer komplicerade handskakningar som SSL/TLS. Utöver säkerhetsrisker finns också implementeringsdetaljer som hantering av minne och headers i C++, och bibliotek som Curl och dess olika wrappers som förenklar HTTP-kommunikation.

## Se även:
- cURL biblioteket: https://curl.haxx.se/libcurl/
- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': https://tools.ietf.org/html/rfc7617
- Bas64 kodning: https://en.wikipedia.org/wiki/Base64
- Säkra autentiseringsmetoder: https://owasp.org/www-community/Authentication_Cheat_Sheet