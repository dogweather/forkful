---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:01.096203-07:00
description: "Hoe: Het gereedschap bij uitstek voor deze klus? `curl`. Het is een\
  \ krachtige opdrachtregel-tool die gegevens van het web haalt. Hier is het eenvoudigste\u2026"
lastmod: '2024-04-05T22:40:36.920140-06:00'
model: gpt-4-0125-preview
summary: Het gereedschap bij uitstek voor deze klus?
title: Een webpagina downloaden
weight: 42
---

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
