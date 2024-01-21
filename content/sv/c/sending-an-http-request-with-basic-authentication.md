---
title:                "Skicka en HTTP-förfrågan med Basic-autentisering"
date:                  2024-01-20T18:01:11.748898-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en HTTP-förfrågan med Basic-autentisering"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
I C programmering, innebär att skicka en HTTP-förfrågan med basautentisering att du lägger till användarnamn och lösenord i en förfrågan för att komma åt skyddat innehåll. Programmerare gör detta för att interagera med webbresurser som kräver säker åtkomst.

## Hur man gör:
För att skicka en HTTP-förfrågan med basautentisering i C, kan du använda biblioteket `libcurl`. Här är ett grundläggande exempel:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(int argc, char *argv[]) {
    CURL *curl;
    CURLcode res;
    
    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();
    
    if(curl) {
        // Ställer in URL och autentiseringsuppgifter
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/data");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERNAME, "användarnamn");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "lösenord");
        
        // Skickar förfrågan och hanterar svar
        res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() misslyckades: %s\n", 
                    curl_easy_strerror(res));
        }
        curl_easy_cleanup(curl);
    }
    
    curl_global_cleanup();
    return 0;
}
```
**Output** kan variera beroende på serverns svar eller statuskoderna.

## Djupdykning:
Historiskt har HTTP-basautentisering varit en enkel metod för att begränsa åtkomst till webbresurser. Den är inte det säkraste alternativet men är lätt att implementera. I modern tid används ofta tokens och OAuth för större säkerhet. Med `libcurl`, som stöder dessa tekniker, är det dock fortfarande möjligt att använda basautentisering där det är lämpligt. Detaljer du bör tänka på inkluderar att autentiseringsuppgifter ska överföras över säkra anslutningar (HTTPS), och lösenord bör aldrig hårdkodas i produktionskod.

## Se även:
- cURL bibliotekets officiella dokumentation: https://curl.se/libcurl/c/libcurl.html
- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': https://tools.ietf.org/html/rfc7617
- Säkra autentiseringsmetoder med OAuth: https://oauth.net/