---
title:                "Laste ned en nettside"
date:                  2024-02-03T17:56:01.166716-07:00
model:                 gpt-4-0125-preview
simple_title:         "Laste ned en nettside"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/downloading-a-web-page.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å laste ned en nettside i C innebærer å programmert få tilgang til innholdet på en nettside over internett og lagre det lokalt for behandling eller offline bruk. Programmerere engasjerer seg ofte i dette for å konsumere webtjenester, skrape nettinnhold eller samhandle direkte med online ressurser fra sine applikasjoner.

## Hvordan:

For å laste ned en nettside i C, er en populær tilnærming å bruke libcurl-biblioteket, et effektivt og bærbart klient-side URL-overføringsbibliotek. Sørg for at du har installert og linket libcurl i prosjektet ditt. Her er et eksempel som demonstrerer hvordan man bruker libcurl for å laste ned innholdet på en nettside:

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

    curl = curl_easy_init(); // Initialiser en libcurl enkel økt
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data); // Tilbakeringing for å skrive mottatte data
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp); // Sett filpekeren for å skrive dataene til

        res = curl_easy_perform(curl); // Utfør filnedlastingen
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));
        }

        /* alltid rydde opp */
        curl_easy_cleanup(curl); // Rengjør den enkle økten
        fclose(fp); // Lukke filstrømmen
    }
    return 0;
}
```
Eksempelutdata (ingen synlig utdata i konsollen): Denne koden laster ned innholdet på den angitte URL-en og lagrer det i en fil med navnet `downloaded_page.html`. Sjekk programmets katalog for denne filen for å se det nedlastede innholdet.

## Dypdykk:

Historisk sett var nedlasting av webinnhold i C mer omstendelig, og krevde manuell socket-programmering og håndtering av HTTP-protokollen. Libcurl abstraherer disse kompleksitetene, og tilbyr et robust og høynivå API for dataoverføring over nettet.

Selv om libcurl forenkler HTTP-forespørsler i C, kan moderne programmeringsspråk som Python med sitt `requests`-bibliotek eller JavaScript (Node.js) med ulike HTTP-klientbiblioteker tilby mer intuitiv syntaks og innebygd støtte for JSON og andre dataformater som ofte brukes i webkommunikasjon. Imidlertid gir C og libcurl en høytytende og stabil løsning for systemer der effektivitet, finjustert kontroll eller integrasjon i eksisterende C-kodebaser er kritisk. Det er også verdt å merke seg at C, kombinert med libcurl, kan brukes til mer enn bare å laste ned nettsider - det er i stand til FTP, SMTP og mye mer, noe som gjør det til et allsidig verktøy i en programmerers verktøykasse.
