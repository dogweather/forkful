---
title:                "Een webpagina downloaden"
aliases: - /nl/bash/downloading-a-web-page.md
date:                  2024-01-28T21:59:01.096203-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een webpagina downloaden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/bash/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het downloaden van een webpagina betekent het ophalen van de gegevens van het internet en deze lokaal opslaan. Programmeurs doen dit voor webscraping, offline analyse of back-updoeleinden.

## Hoe:
Het gereedschap bij uitstek voor deze klus? `curl`. Het is een krachtige opdrachtregel-tool die gegevens van het web haalt. Hier is het eenvoudigste gebruiksscenario:

```Bash
curl https://example.com -o webpage.html
```

Dit commando downloadt de HTML van `example.com` en schrijft deze naar een bestand met de naam `webpage.html`. Bekijk de output:

```Bash
# Voorbeeld van output
  % Totaal    % Ontvangen % Xferd  Gemiddelde Snelheid   Tijd    Tijd     Tijd  Huidig
                                 Dload  Upload   Totaal   Besteed  Over  Snelheid
100  1256  100  1256    0     0   6458      0 --:--:-- --:--:-- --:--:--  6497
```

Wil je zien wat je in real-time aan het downloaden bent? Laat de `-o` weg en de download wordt direct in je console weergegeven:

```Bash
curl https://example.com
```

## Diep Duiken
`curl` bestaat al sinds 1997, en heeft zijn niche gevonden voor weboperaties. Waarom `curl` boven browser downloads? Automatisering en script-vriendelijkheid. Het is niet-interactief en kan gemakkelijk worden verweven in bash scripts.

Het vermelden waard alternatieven: `wget`, een andere commando-regel krachtpatser die webpagina's recursief kan downloaden. Voor zware scraping of wanneer een echte browsercontext nodig is, wenden programmeurs zich tot tools als Selenium, Puppeteer of Scrapy.

Dieper ingaan op de werking van `curl`: het ondersteunt meerdere protocollen, van HTTP en HTTPS tot FTP, en een heleboel opties (--header, --cookie, --user-agent, etc.) voor het fijnafstemmen van verzoeken. Plus, het komt meestal vooraf geïnstalleerd op op Unix-gebaseerde systemen.

## Zie Ook
- Curl Documentatie: https://curl.haxx.se/docs/manpage.html
- Wget Handleiding: https://www.gnu.org/software/wget/manual/wget.html
- Introductie tot webscraping met Python: https://realpython.com/python-web-scraping-practical-introduction/
