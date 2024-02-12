---
title:                "Een webpagina downloaden"
aliases:
- /nl/c/downloading-a-web-page.md
date:                  2024-02-03T17:55:49.788099-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een webpagina downloaden"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/downloading-a-web-page.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een webpagina downloaden in C houdt in dat je programmatisch toegang krijgt tot de inhoud van een webpagina via het internet en deze lokaal opslaat voor verwerking of offline gebruik. Programmeurs doen dit vaak om webdiensten te gebruiken, webinhoud te scrapen of direct vanuit hun applicaties interactie te hebben met online bronnen.

## Hoe te:

Om een webpagina in C te downloaden, is een populaire aanpak het gebruik van de libcurl-bibliotheek, een efficiënte en draagbare client-side URL-transferbibliotheek. Zorg ervoor dat je libcurl hebt geïnstalleerd en gelinkt in je project. Hier is een voorbeeld dat laat zien hoe te libcurl gebruiken om de inhoud van een webpagina te downloaden:

```c
#include <stdio.h>
#include <curl/curl.h>

size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t geschreven = fwrite(ptr, size, nmemb, stream);
    return geschreven;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://example.com";
    char outfilename[FILENAME_MAX] = "./downloaded_page.html";

    curl = curl_easy_init(); // Initialiseer een libcurl easy sessie
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data); // Callback voor het schrijven van ontvangen gegevens
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp); // Stel de bestandspointer in om de gegevens naar te schrijven

        res = curl_easy_perform(curl); // Voer de bestandsdownload uit
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() mislukt: %s\n",
                    curl_easy_strerror(res));
        }

        /* altijd opruimen */
        curl_easy_cleanup(curl); // Ruim de easy sessie op
        fclose(fp); // Sluit de bestandsstroom
    }
    return 0;
}
```
Voorbeelduitvoer (geen zichtbare uitvoer in de console): Deze code downloadt de inhoud van de opgegeven URL en slaat deze op in een bestand genaamd `downloaded_page.html`. Controleer de map van je programma voor dit bestand om de gedownloade inhoud te zien.

## Diepgaande duik:

Historisch gezien was het downloaden van webinhoud in C omslachtiger, wat handmatige socketprogrammering en HTTP-protocolbehandeling vereiste. Libcurl abstraheert deze complexiteiten en biedt een robuuste en hoogwaardige API voor gegevensoverdracht via het web.

Hoewel libcurl HTTP-verzoeken in C vereenvoudigt, kunnen moderne programmeertalen zoals Python met hun `requests`-bibliotheek of JavaScript (Node.js) met verschillende HTTP-clientbibliotheken een meer intuïtieve syntax en ingebouwde ondersteuning bieden voor JSON en andere gegevensformaten die vaak worden gebruikt in webcommunicatie. Echter, C en libcurl bieden een hoogwaardige en stabiele oplossing voor systemen waar efficiëntie, gedetailleerde controle of integratie in bestaande C-codebases cruciaal zijn. Het is ook vermeldenswaard dat C, gecombineerd met libcurl, kan worden gebruikt voor meer dan alleen het downloaden van webpagina's - het is in staat tot FTP, SMTP, en nog veel meer, waardoor het een veelzijdig hulpmiddel is in de gereedschapskist van een programmeur.
