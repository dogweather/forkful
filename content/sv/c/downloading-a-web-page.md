---
title:                "Nerladdning av en webbsida"
html_title:           "C: Nerladdning av en webbsida"
simple_title:         "Nerladdning av en webbsida"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför
Det finns många anledningar till varför någon skulle vilja ladda ner en webbsida. Kanske vill man spara en kopia för framtida referens, analysera dess struktur eller innehåll, eller helt enkelt för att ha tillgång till den offline.

## Så här gör du
Att ladda ner en webbsida i C är en enkel process. Först och främst behöver vi inkludera nödvändiga bibliotek, såsom <stdio.h> och <curl/curl.h>. Sedan behöver vi skapa en `FILE`-pekar för att kunna skriva till en fil. Sedan kan vi använda Curl bibliotekets `curl_easy_init`, `curl_easy_setopt` och `curl_easy_perform` för att hämta webbsidan och skriva innehållet till filen.

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
  // Skapa en FILE-pekar och öppna en fil för att skriva innehållet till
  FILE *fp;
  fp = fopen("webbsida.html", "w");
  
  // Skapa en Curl-session och ställ in URL:en
  CURL *curl;
  curl = curl_easy_init();
  curl_easy_setopt(curl, CURLOPT_URL, "https://www.exempel.se/");
  
  // Ange filen som ska ta emot datan
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
  
  // Hämta webbsidan och spara innehållet till filen
  CURLcode res = curl_easy_perform(curl);
  
  // Stäng filen och rensa upp efter oss
  fclose(fp);
  curl_easy_cleanup(curl);
  return 0;
}
```

### Sample Output
Kör programmet ovan så kommer du att få en fil som heter "webbsida.html" som innehåller allt innehåll från webbsidan https://www.exempel.se/.

## Djupdykning
För att förstå mer om hur hämtning av webbsidor fungerar i C, kan det vara bra att titta närmare på Curl bibliotekets funktioner och parametrar. Till exempel kan vi använda `curl_easy_setopt` för att ställa in ytterligare inställningar, som t.ex. användaragent, tidsgräns för hämtning, och funktionspekare för att hantera nedladdningen.

See Also
- <https://curl.haxx.se/libcurl/c/>
- <https://curl.se/libcurl/c/code.html>