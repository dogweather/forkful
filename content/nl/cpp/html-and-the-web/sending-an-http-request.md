---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:23.046191-07:00
description: "Het versturen van een HTTP-verzoek haalt gegevens op van een webserver.\
  \ Programmeurs doen dit om te interageren met webservices, informatie te verzamelen\u2026"
lastmod: '2024-02-25T18:49:48.443148-07:00'
model: gpt-4-0125-preview
summary: "Het versturen van een HTTP-verzoek haalt gegevens op van een webserver.\
  \ Programmeurs doen dit om te interageren met webservices, informatie te verzamelen\u2026"
title: Een HTTP-verzoek verzenden
---

{{< edit_this_page >}}

## Wat & Waarom?
Het versturen van een HTTP-verzoek haalt gegevens op van een webserver. Programmeurs doen dit om te interageren met webservices, informatie te verzamelen of te communiceren tussen systemen.

## Hoe:

```C++
#include <iostream>
#include <cpr/cpr.h> // Zorg ervoor dat je eerst de CPR-bibliotheek installeert

int main() {
    cpr::Response r = cpr::Get(cpr::Url{"http://httpbin.org/get"});
    std::cout << r.text << std::endl; // Geeft het antwoord van het lichaam weer
    return 0;
}
```

Voorbeelduitvoer:
```json
{
  "args": {},
  "headers": {
    "Accept": "*/*",
    "Host": "httpbin.org",
    "User-Agent": "curl/7.64.1"
  },
  "origin": "0.0.0.0",
  "url": "https://httpbin.org/get"
}
```

## Diepduik
HTTP-verzoeken zijn cruciaal geweest sinds de komst van het web; ze volgen een client-servermodel. Vóór C++-bibliotheken zoals CPR betekende het versturen van HTTP-verzoeken meestal direct gebruikmaken van `libcurl`, of integratie met een andere taal die beter uitgerust was voor webcommunicatie.

CPR, wat staat voor C++ Requests, is een eenvoudige te gebruiken wrapper, geïnspireerd door Python's `requests` module. Het abstraheert veel van de complexiteiten van `libcurl`. Alternatieven bestaan nog steeds, zoals Boost.Beast voor low-level HTTP/S-bewerkingen, of POCO-bibliotheken die draagbaarheid bieden.

Een duik onder de motorkap nemen, betekent dat het versturen van een HTTP-verzoek het opzetten van een TCP-verbinding inhoudt, het formatteren van een verzoek in overeenstemming met het HTTP-protocol, en vervolgens het parseren van het antwoord. Dit goed krijgen vanaf nul is niet triviaal vanwege foutafhandeling, complexiteiten met HTTP-versies en beveiligingsoverwegingen.

## Zie Ook

- CPR Github Repository: https://github.com/libcpr/cpr
- `libcurl`-documentatie voor geavanceerder gebruik: https://curl.se/libcurl/
- Officiële Boost.Beast-documentatie: https://www.boost.org/doc/libs/release/libs/beast/
- POCO C++-bibliotheken documentatie: https://pocoproject.org/docs/
