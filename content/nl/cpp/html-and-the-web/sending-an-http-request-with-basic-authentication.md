---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:01.190727-07:00
description: "Een HTTP-verzoek met basisauthenticatie versturen houdt in dat een gebruikersnaam\
  \ en wachtwoord aan een verzoek worden toegevoegd voor toegangscontrole.\u2026"
lastmod: '2024-03-13T22:44:51.112355-06:00'
model: gpt-4-0125-preview
summary: Een HTTP-verzoek met basisauthenticatie versturen houdt in dat een gebruikersnaam
  en wachtwoord aan een verzoek worden toegevoegd voor toegangscontrole.
title: Een HTTP-verzoek verzenden met basisauthenticatie
weight: 45
---

## Wat & Waarom?

Een HTTP-verzoek met basisauthenticatie versturen houdt in dat een gebruikersnaam en wachtwoord aan een verzoek worden toegevoegd voor toegangscontrole. Programmeurs doen dit voor eenvoudige authenticatieschema's om bronnen op de server te beschermen.

## Hoe:

Hier is een basisvoorbeeld met de `CURL`-bibliotheek in C++. Zorg ervoor dat je `libcurl` hebt geïnstalleerd voordat je begint.

```C++
#include <iostream>
#include <curl/curl.h>

// Eenvoudige callback-functie om gegevens te verwerken die door curl ontvangen zijn
static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL *curl;
    CURLcode res;
    std::string readBuffer;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://jouwapi.com/data");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERNAME, "gebruiker");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "wachtwoord");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        
        // Voer het verzoek uit en controleer op fouten
        res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() is mislukt: %s\n", curl_easy_strerror(res));
        } else {
            std::cout << readBuffer << std::endl;
        }
        
        // Opruimen
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Je zult een reactie van de server zien die naar de console wordt afgedrukt, ervan uitgaande dat er geen fouten zijn opgetreden.

## Diepgaand

Basisauthenticatie is ouderwets en gaat terug tot de vroege dagen van HTTP. Tegenwoordig heeft de industrie een voorkeur voor veiligere methoden zoals OAuth en tokens. Ondanks dit blijft basisauthenticatie in gebruik, vaak voor interne of eenvoudige systemen waar zware beveiligingslagen overbodige overkill zijn.

Onder de motorkap worden je gebruikersnaam en wachtwoord base64-gecodeerd en in de HTTP-header gestopt. Het is eenvoudig maar onveilig als het niet over HTTPS gaat, omdat base64 gemakkelijk omkeerbaar is - wat HTTPS een must maakt.

Als `libcurl` niet naar jouw smaak is, overweeg dan alternatieven zoals de `cpp-httplib`-bibliotheek, of je kunt met `Boost.Beast` gaan voor een meer hands-on benadering.

## Zie Ook

- [libcurl](https://curl.se/libcurl/)
- [cpp-httplib GitHub-repository](https://github.com/yhirose/cpp-httplib)
- [Boost.Beast-documentatie](https://www.boost.org/doc/libs/master/libs/beast/doc/html/index.html)
