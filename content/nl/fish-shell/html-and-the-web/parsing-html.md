---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:29.110491-07:00
description: "Hoe te: Fish Shell is niet de eerste keuze voor het parsen van HTML,\
  \ maar met de juiste tools is het haalbaar. Laten we `pup`, een command-line HTML-\u2026"
lastmod: '2024-03-13T22:44:51.245170-06:00'
model: gpt-4-0125-preview
summary: Fish Shell is niet de eerste keuze voor het parsen van HTML, maar met de
  juiste tools is het haalbaar.
title: HTML Parsen
weight: 43
---

## Hoe te:
Fish Shell is niet de eerste keuze voor het parsen van HTML, maar met de juiste tools is het haalbaar. Laten we `pup`, een command-line HTML-parser, gebruiken om met HTML-inhoud te werken.

```fish
# Eerst, installeer pup
brew install pup

# Haal de titel op van example.com
curl -s http://example.com | pup 'title text{}'

# Voorbeelduitvoer zou de titel van de website moeten zijn, iets zoals:
# Voorbeeld Domein
```

Nu laten we alle hyperlinks vangen:

```fish
# Extraheren links (href attributen) van example.com
curl -s http://example.com | pup 'a attr{href}'

# Voorbeelduitvoer:
# http://www.iana.org/domains/example
```

## Diepgaand
Voor Fish Shell en `pup` zouden mensen klungelige regex of complexe server-side scripts gebruiken. Tools zoals `pup` hebben het proces verfijnd, steunend op CSS-selector syntax voor een intuïtievere en betrouwbaardere parsing.

Alternatieven omvatten Python's Beautiful Soup of Node.js met Cheerio; ze zijn krachtiger maar niet zo beknopt voor one-liners.

HTML parsen met Fish komt neer op het uitbesteden van de taak aan gespecialiseerde tools vanwege zijn beperkte tekstmanipulatiecapaciteiten. Fish roept deze tools aan, vangt hun uitvoer op en laat je je scriptmagie werken.

## Zie Ook
- [Pup GitHub Repo](https://github.com/ericchiang/pup) - Documentatie en voorbeelden.
- [Fish Shell Documentatie](https://fishshell.com/docs/current/index.html) - Leer meer over Fish.
- [Beautiful Soup Documentatie](https://www.crummy.com/software/BeautifulSoup/bs4/doc/) - Voor meer complexe HTML parsing in Python.
- [Cheerio GitHub Repo](https://github.com/cheeriojs/cheerio) - Voor degenen die geïnteresseerd zijn in een op JavaScript gebaseerde aanpak.
