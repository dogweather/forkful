---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:09.308651-07:00
description: "Een HTTP-verzoek met basisauthenticatie verzenden in C omvat het opstellen\
  \ van een HTTP-verzoek dat een Authorization-header bevat met gebruikersgegevens\u2026"
lastmod: '2024-03-11T00:14:25.152811-06:00'
model: gpt-4-0125-preview
summary: "Een HTTP-verzoek met basisauthenticatie verzenden in C omvat het opstellen\
  \ van een HTTP-verzoek dat een Authorization-header bevat met gebruikersgegevens\u2026"
title: Een HTTP-verzoek verzenden met basisauthenticatie
---

{{< edit_this_page >}}

## Wat & Waarom?
Een HTTP-verzoek met basisauthenticatie verzenden in C omvat het opstellen van een HTTP-verzoek dat een Authorization-header bevat met gebruikersgegevens gecodeerd in Base64. Dit is een gangbare methode om een eenvoudige authenticatielaag toe te voegen aan HTTP-verzoeken, waardoor beperkte bronnen programmatisch toegankelijk worden.

## Hoe te:
Om een HTTP-verzoek met basisauthenticatie in C te verzenden, moeten we de libcurl-bibliotheek gebruiken, een populaire, veelzijdige en gebruiksvriendelijke client-zijde URL-overdrachtsbibliotheek. Het hanteert verschillende protocollen, inclusief HTTP en HTTPS, wat onze taak vereenvoudigt. Zorg ervoor dat libcurl is geïnstalleerd in je systeem voordat je verdergaat. Hier is een basisvoorbeeld dat laat zien hoe je een GET-verzoek met basisauthenticatie verstuurt:

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *krul;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    krul = curl_easy_init();
    if(krul) {
        // De URL waar het verzoek naar wordt verzonden
        curl_easy_setopt(krul, CURLOPT_URL, "http://example.com/resource");
        // Het inschakelen van het gebruik van basisauthenticatie
        curl_easy_setopt(krul, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        // Het verstrekken van de gebruikersnaam en wachtwoord voor de basisauthenticatie
        curl_easy_setopt(krul, CURLOPT_USERPWD, "gebruikersnaam:wachtwoord");

        // Het uitvoeren van het GET-verzoek
        res = curl_easy_perform(krul);

        // Controleren op fouten
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() mislukt: %s\n",
                    curl_easy_strerror(res));

        // Altijd opruimen
        curl_easy_cleanup(krul);
    }
    
    curl_global_cleanup();

    return 0;
}
```
Vervang in het bovenstaande voorbeeld `"http://example.com/resource"`, `"gebruikersnaam"`, en `"wachtwoord"` met je daadwerkelijke URL, gebruikersnaam en wachtwoord.

Deze code initialiseert een `CURL`-object, stelt de URL in, schakelt HTTP Basic Authentication in en geeft de referenties op. Vervolgens verstuurt het het verzoek en ruimt het na zichzelf op. Als het succesvol is, wordt de gevraagde bron opgehaald; als er een fout is, wordt deze naar stderr afgedrukt.

Voorbeeldaanvoer (ervan uitgaande dat de authenticatie succesvol is en de toegang tot de bron is) wordt mogelijk niet direct door het programma getoond, aangezien het voorbeeld voornamelijk het verzenden van de aanvraag demonstreert. Om de respons af te drukken, zou je het programma uitbreiden om de HTTP-responsgegevens te verwerken.

## Diepgaande duik:
HTTP-verzoeken verzenden met basisauthenticatie in C, zoals getoond, maakt gebruik van de libcurl-bibliotheek vanwege zijn robuustheid en eenvoud. Historisch gezien was het opstellen van HTTP-verzoeken puur in C zonder dergelijke bibliotheken omslachtig en foutgevoelig, waarbij programmeerwerk op laag niveau met sockets en handmatige constructie van HTTP-headers noodzakelijk was.

Basisauthenticatie zelf is een methode uit de begindagen van het web. Het verzendt referenties in een makkelijk decodeerbaar formaat (Base64), dat inherent onveilig is over platte tekstkanalen. Moderne toepassingen geven vaak de voorkeur aan veiligere authenticatiemethoden, zoals OAuth 2.0 of JWT (JSON Web Tokens), vooral voor gevoelige gegevens.

Echter, voor interne, minder kritieke systemen, of snelle en vuile scripts waar gemak zwaarder weegt dan de beveiligingsproblemen, blijft basisauth in gebruik. Bovendien, wanneer gecombineerd met versleutelde verbindingen (HTTPS), wordt de eenvoud ervan een voordeel voor snelle ontwikkeling, testen of automatiseringswerk waar geavanceerdere beveiligingsmechanismen niet zo noodzakelijk zijn.

In contexten waar cutting-edge beveiliging niet onderhandelbaar is, moeten alternatieven zoals token-gebaseerde authenticatie worden geprioriteerd. Desondanks biedt begrijpen hoe basisauth in C te implementeren via libcurl een fundamentele vaardigheid die kan worden aangepast aan verschillende authenticatiemethoden en protocollen, wat de genuanceerde afwegingen reflecteert tussen beveiliging, gemak en applicatievereisten in webontwikkeling.
