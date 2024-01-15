---
title:                "Skicka en http-begäran"
html_title:           "C: Skicka en http-begäran"
simple_title:         "Skicka en http-begäran"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Varför

Så varför skulle man egentligen bry sig om att skicka en HTTP-förfrågan? Jo, det är ett viktigt sätt att kommunicera med webbtjänster och göra olika typer av förfrågningar, såsom att hämta data eller skicka data till en server.

# Hur man gör det

För att skicka en HTTP-förfrågan i C behöver vi använda oss av biblioteket libcurl. Detta bibliotek gör det enkelt för oss att skapa och skicka förfrågningar, samt hantera svaren från servern.

Vi börjar med att inkludera libcurl i vår kod genom att lägga till följande rad:

```C
#include <curl/curl.h>
```

För att skicka en enkel GET-förfrågan använder vi funktionen `curl_easy_perform()`, som tar emot en URL som argument. Här är ett exempel på hur vår kod kan se ut:

```C
// Skapa en CURL-handle
CURL *curl;
// Skapa en variabel för att lagra resultatet av vår förfrågan
CURLcode res;
// Skapa en variabel för att lagra den önskade URL:en
char *url = "https://www.example.com";
// Initiera CURL och lagra det i vår CURL-handle
curl = curl_easy_init();
if(curl) {
  // Ange URL:en
  curl_easy_setopt(curl, CURLOPT_URL, url);
  // Skicka förfrågan
  res = curl_easy_perform(curl);
  // Om allt går bra skriver vi ut statuskoden från svaret
  if(res == CURLE_OK)
    printf("Förfrågan skickades med statuskod: %d\n", res);
  // Frigör vår CURL-handle
  curl_easy_cleanup(curl);
}
```

Om allt går som det ska, bör vi se en utskrift som säger "Förfrågan skickades med statuskod: 200", vilket betyder att vår förfrågan var framgångsrik och att vi har fått ett ok-svar från servern.

# Djupdykning

Om vi vill göra mer avancerade saker med vår förfrågan, såsom att skicka data eller sätta olika typer av headers, kan vi använda fler funktioner från libcurl. Till exempel kan vi använda `curl_easy_setopt()` för att ställa in önskade headers eller använda `curl_easy_setopt()` för att skicka data med vår förfrågan.

Det är också möjligt att hantera eventuella fel som kan uppstå under förfrågan och vidta åtgärder beroende på vad som har gått fel. Vi kan använda funktionen `curl_easy_strerror()` för att få en beskrivning av felet, vilket kan vara till hjälp när vi försöker felsöka vår kod.

# Se även

Här är några länkar som kan vara användbara för vidare läsning:

- Officiell dokumentation för libcurl: https://curl.se/libcurl/
- En fullständig guide för att använda libcurl i C: https://curl.se/libcurl/c/
- Ett praktiskt verktyg för att testa HTTP-förfrågningar: https://httpbin.org/