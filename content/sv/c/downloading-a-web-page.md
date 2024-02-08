---
title:                "Ladda ner en webbsida"
aliases:
- sv/c/downloading-a-web-page.md
date:                  2024-02-03T17:56:27.572730-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ladda ner en webbsida"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/downloading-a-web-page.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att ladda ner en webbsida i C innebär att programmatiskt få tillgång till innehållet på en webbsida över internet och spara det lokalt för bearbetning eller offlineanvändning. Programmerare engagerar sig ofta i detta för att konsumera webbtjänster, skrapa webbinnehåll eller interagera direkt med online-resurser från sina applikationer.

## Hur man gör:

För att ladda ner en webbsida i C är en populär metod att använda libcurl-biblioteket, ett effektivt och portabelt klient-sida URL överföringsbibliotek. Se till att du har libcurl installerat och länkat i ditt projekt. Här är ett exempel som visar hur man använder libcurl för att ladda ner innehållet på en webbsida:

```c
#include <stdio.h>
#include <curl/curl.h>

size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t written = fwrite(ptr, size, nmemb, stream);
    return written;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://example.com";
    char outfilename[FILENAME_MAX] = "./downloaded_page.html";

    curl = curl_easy_init(); // Initiera en libcurl easy session
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data); // Callback för att skriva mottagna data
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp); // Ställ in filpekaren för att skriva datan till

        res = curl_easy_perform(curl); // Utför filnedladdningen
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));
        }

        /* alltid städa upp */
        curl_easy_cleanup(curl); // Städa upp den enkla sessionen
        fclose(fp); // Stäng filströmmen
    }
    return 0;
}
```
Exempelutskrift (ingen synlig utskrift i konsolen): Den här koden laddar ner innehållet på angiven URL och sparar det till en fil med namnet `downloaded_page.html`. Kontrollera programmets katalog för denna fil för att se det nedladdade innehållet.

## Djupdykning:

Historiskt sett var nedladdning av webbinnehåll i C mer omständligt, krävde manuell socket-programmering och hantering av HTTP-protokollet. Libcurl abstraherar dessa komplexiteter och erbjuder ett robust och högnivå-API för datatransfer över webben.

Medan libcurl förenklar HTTP-förfrågningar i C, kan moderna programmeringsspråk som Python med deras `requests`-bibliotek eller JavaScript (Node.js) med diverse HTTP-klientbibliotek erbjuda mer intuitiv syntax och inbyggt stöd för JSON och andra dataformat som ofta används i webbkommunikation. Dock erbjuder C och libcurl en prestandastark och stabil lösning för system där effektivitet, finjusterad kontroll eller integration i befintliga C-kodbaser är kritisk. Det är också värt att notera att C, kombinerat med libcurl, kan användas för mer än bara att ladda ner webbsidor – det är kapabelt till FTP, SMTP och mycket mer, vilket gör det till ett mångsidigt verktyg i en programmerares verktygslåda.
