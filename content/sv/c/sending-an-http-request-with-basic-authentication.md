---
title:                "Sända en http-förfrågan med grundläggande autentisering"
html_title:           "C: Sända en http-förfrågan med grundläggande autentisering"
simple_title:         "Sända en http-förfrågan med grundläggande autentisering"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

Basic authentication är en vanlig autentiseringsteknik för att säkert kommunicera mellan två datorer med hjälp av HTTP-protokollet. Det är viktigt att förstå hur man skickar en HTTP-begäran med basic authentication för att kunna använda den på ett effektivt sätt.

## Hur man gör det

```C
#include <stdio.h> 
#include <curl/curl.h>

// Definiera API-endpointet för begäran
#define API_ENDPOINT "https://www.example.com/api"

// Definiera användarnamn och lösenord för basic authentication
#define USERNAME "användarnamn"
#define PASSWORD "lösenord"

// Gör en HTTP-begäran med basic authentication
// Använder CURL-biblioteket
CURL *curl = curl_easy_init();
if(curl) {
  // Sätt inloggningsuppgifter
  curl_easy_setopt(curl, CURLOPT_USERNAME, USERNAME);
  curl_easy_setopt(curl, CURLOPT_PASSWORD, PASSWORD);
  
  // Ange API-endpointet
  curl_easy_setopt(curl, CURLOPT_URL, API_ENDPOINT);
  
  // Skicka begäran och få svar
  CURLcode res = curl_easy_perform(curl);
  
  // Kontrollera eventuella fel
  if(res != CURLE_OK)
    fprintf(stderr, "curl_easy_perform() failed: %s\n",
            curl_easy_strerror(res));
  
  // Stäng CURL-anslutning
  curl_easy_cleanup(curl);
}
```

Exempel på utmatning från en lyckad begäran:
```
HTTP/1.1 200 OK
Content-Type: application/json

{
  "status": "success",
  "message": "Din begäran genomfördes korrekt"
}
```

## Djupdykning

För att skicka en HTTP-begäran med basic authentication behöver man skapa en headerrad som innehåller inloggningsuppgifterna i formatet "användarnamn:lösenord" och koda den i base64. Detta läggs sedan till i begäran som en standard "Authorization" header. En annan viktig aspekt att ta hänsyn till är säkerheten i överföringen, eftersom basic authentication skickar inloggningsuppgifterna i klartext.

## Se även

- [CURL dokumentation](https://curl.se/libcurl/c/)
- [HTTP-basautentisering](https://developer.mozilla.org/sv/docs/Web/HTTP/Authentication#basic_authentication_scheme) (Mozilla utvecklarnätverk)