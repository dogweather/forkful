---
title:                "C: Sända en http-begäran"
simple_title:         "Sända en http-begäran"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

I dagens digitala värld är det vanligt att program måste kommunicera med andra system över internet. För att göra detta behöver man ibland skicka en förfrågan, eller HTTP request, till en annan server. Det är väldigt viktigt för en programmerare att förstå hur man skapar och skickar en korrekt HTTP request för att bygga effektiva och tillförlitliga program.

## Hur man gör

För att skicka en HTTP request från ett C-program måste vi använda oss av en extern bibliotek som heter libcurl. Detta bibliotek ger oss alla verktyg som behövs för att enkelt skicka requests. För att börja, måste vi inkludera libcurl i vårt program genom att skriva följande rad längst upp i filen:

```
#include <curl/curl.h>
```

Nästa steg är att initiera libcurl genom att skriva:

```
CURL *curl;
curl = curl_easy_init();
```

Nu är det dags att bygga vår HTTP request. Vi börjar med att ange mål-URL med hjälp av funktionen `curl_easy_setopt()`, där vi anger den inställning vi vill ändra, vilken kan vara URL, metod (GET, POST, PUT, etc.) och mer. Här är ett exempel:

```
curl_easy_setopt(curl, CURLOPT_URL, "https://example.com");
```

För att skicka vår request, använder vi funktionen `curl_easy_perform()`, som tar vår fördefinierade `curl`-variabel och skickar requesten till mål-URL:en. Om requesten var framgångsrik kommer den returnera `CURLE_OK`, annars kommer en felkod returneras. 

```
CURLcode res = curl_easy_perform(curl);
```

Slutligen måste vi städa upp genom att avsluta libcurl och frigöra alla resurser som användes genom att köra `curl_easy_cleanup()`.

```
curl_easy_cleanup(curl);
```

## Djupdykning

En HTTP request består av flera olika delar, inklusive en metod (GET, POST, PUT, etc.), en URL, headers, och ibland även en body. Genom libcurl, kan vi enkelt ändra och lägga till headers och body innan vi skickar vår request. Det är också möjligt att anpassa hur vi vill ta emot svar från servern, till exempel i form av en sträng eller genom att skicka svaret direkt till en given fil.

Det är också viktigt att förstå olika HTTP statuskoder som vi kan få som svar på vår request. Genom att läsa och tolka dessa koder kan vi felsöka och förbättra vår request för att få bättre resultat.

## Se även

- [libcurl dokumentation](https://curl.haxx.se/libcurl/)
- [HTTP statuskoder](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)