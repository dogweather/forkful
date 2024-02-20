---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:37.831684-07:00
description: "Het verzenden van HTTP-verzoeken is een manier om met web servers te\
  \ communiceren door gegevens op te halen of te verzenden naar behoefte. Programmeurs\u2026"
lastmod: 2024-02-19 22:05:10.331097
model: gpt-4-0125-preview
summary: "Het verzenden van HTTP-verzoeken is een manier om met web servers te communiceren\
  \ door gegevens op te halen of te verzenden naar behoefte. Programmeurs\u2026"
title: Een HTTP-verzoek verzenden
---

{{< edit_this_page >}}

## Wat & Waarom?

Het verzenden van HTTP-verzoeken is een manier om met web servers te communiceren door gegevens op te halen of te verzenden naar behoefte. Programmeurs gebruiken HTTP-verzoeken om te interageren met API's of webservices, waardoor applicaties toegang kunnen krijgen tot middelen, diensten en gegevens op het internet.

## Hoe:

Fish heeft geen ingebouwde commando's voor het verzenden van HTTP-verzoeken, maar je kunt `curl` rechtstreeks vanuit de shell gebruiken:

```Fish
curl http://api.example.com/data
```

Voor een POST-verzoek met JSON-gegevens:

```Fish
curl -X POST -H "Content-Type: application/json" -d '{"key":"value"}' http://api.example.com/data
```

Om het antwoord op te slaan:

```Fish
set response (curl -X GET http://api.example.com/data)
```

En dit is wat je misschien ziet na een GET-verzoek:

```Fish
{
  "response": "Sommige gegevens van de server"
}
```

## Diepgaand

Historisch gezien zijn UNIX- en Linux-shells handig voor netwerktaken. In de vroege dagen waren tools zoals `telnet` gebruikelijk voor dergelijke doeleinden. Tegenwoordig zijn hulpprogramma's zoals `curl` en `wget` de standaard. `curl` is een veelzijdige tool die meerdere protocollen ondersteunt en vaak wordt gebruikt vanwege zijn eenvoud en flexibiliteit.

Python of Node.js kunnen worden gebruikt wanneer je meer complexe verzoekafhandeling nodig hebt. Maar voor snelle taken of eenvoudige scripts is `curl` in Fish efficiënt en effectief.

Het implementeren van een HTTP-verzoek via Fish betekent meestal dat je vertrouwt op tools van derden. Fish zelf is ontworpen om een slimme en gebruiksvriendelijke command-line shell te zijn, geen doe-alles-tool. Wanneer je het combineert met de kracht van hulpprogramma's zoals `curl`, krijg je het beste van twee werelden: de bruikbaarheid van Fish en de capaciteiten van `curl`.

## Zie Ook

- Leer meer over `curl`: https://curl.se/docs/manual.html
- Documentatie van de Fish Shell: https://fishshell.com/docs/current/index.html
- Overzicht van HTTP-basics: https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview
- Verken API's met `httpie`, een alternatief voor `curl`: https://httpie.io/
