---
aliases:
- /nl/c/sending-an-http-request/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:08:23.427753-07:00
description: "Het verzenden van een HTTP-verzoek houdt in dat je een verzoek cre\xEB\
  ert en verstuurt naar een webserver om gegevens op te halen of in te dienen.\u2026"
lastmod: 2024-02-18 23:09:02.369276
model: gpt-4-0125-preview
summary: "Het verzenden van een HTTP-verzoek houdt in dat je een verzoek cre\xEBert\
  \ en verstuurt naar een webserver om gegevens op te halen of in te dienen.\u2026"
title: Een HTTP-verzoek verzenden
---

{{< edit_this_page >}}

## Wat & Waarom?

Het verzenden van een HTTP-verzoek houdt in dat je een verzoek creëert en verstuurt naar een webserver om gegevens op te halen of in te dienen. Programmeurs doen dit in C om te interageren met web-API's, webpagina's te downloaden of direct vanuit hun applicaties te communiceren met andere genetwerkte diensten.

## Hoe te:

Om een HTTP-verzoek in C te verzenden, vertrouw je doorgaans op bibliotheken zoals libcurl, aangezien C geen ingebouwde ondersteuning heeft voor webprotocollen. Hier is een eenvoudig voorbeeld waarbij libcurl wordt gebruikt om een GET-verzoek uit te voeren:

Zorg eerst dat je libcurl op je systeem hebt geïnstalleerd. Voeg vervolgens de nodige headers toe en link tegen de libcurl-bibliotheek in je bronbestand:

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init(); // Initialiseer een libcurl-handle
    if(curl) {
        // Stel de URL in die de libcurl-handle ontvangt
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        // Definieer een callback om de gegevens te krijgen
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL); 
        
        // Voer het verzoek uit, res zal de retourcode krijgen
        res = curl_easy_perform(curl);
        // Controleer op fouten
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() is mislukt: %s\n",
                    curl_easy_strerror(res));

        // Ruim altijd op
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Compileer dit met iets zoals `gcc -o http_request http_request.c -lcurl`, het uitvoeren hiervan zou een eenvoudig GET-verzoek naar "http://example.com" moeten uitvoeren.

### Voorbeelduitvoer

Aangezien het voorbeeld de respons van de server niet verwerkt, zal het uitvoeren ervan geen zichtbare uitvoer produceren, behalve mogelijke foutmeldingen. Het integreren van de callback-functie voor het verwerken van ontvangen gegevens is essentieel voor zinvolle interactie.

## Diepere duik

Het concept van het verzenden van HTTP-verzoeken vanuit een C-programma leunt op de krachtige netwerkmogelijkheden van de taal, in combinatie met externe bibliotheken, aangezien C zelf een low-level taal is zonder ingebouwde ondersteuning voor hoogwaardige internetprotocollen. Historisch gezien zouden programmeurs handmatig socketprogrammering in C gebruiken, een complex en tijdrovend proces, om met webservers te interageren voordat speciale bibliotheken zoals libcurl beschikbaar kwamen.

Libcurl, gebouwd bovenop C, stroomlijnt het proces door de lastige details van socketprogrammering en de specificaties van het HTTP-protocol weg te abstraheren. Het ondersteunt een veelvoud aan protocollen buiten HTTP/HTTPS, waaronder FTP, SMTP en meer, waardoor het een veelzijdige tool is voor netwerkprogrammering in C.

Hoewel het gebruik van libcurl voor HTTP-verzoeken in C praktisch is, neigt de moderne programmering vaak naar talen met ingebouwde ondersteuning voor dergelijke taken, zoals Python (requests-bibliotheek) of JavaScript (Fetch API). Deze alternatieven bieden een eenvoudigere, beter leesbare syntaxis ten koste van de gedetailleerde controle en prestatieoptimalisaties die mogelijk zijn in C door directe socketmanipulatie en zorgvuldig afgestemd bibliotheekgebruik.

Voor kritieke prestatietoepassingen of waar directe interactie op systeemniveau noodzakelijk is, blijft C een levensvatbare optie, vooral met libcurl die de complexiteit van webcommunicatie verzacht. Echter, voor de meeste hoogwaardige webinteracties kan het verkennen van meer toegewijde webprogrammeertalen efficiënter blijken.
