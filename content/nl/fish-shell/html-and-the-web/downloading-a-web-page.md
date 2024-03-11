---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:27.873137-07:00
description: "Een webpagina downloaden haalt de inhoud ervan van het web naar je lokale\
  \ machine. Programmeurs doen dit voor automatisering, data scraping, offline\u2026"
lastmod: '2024-03-11T00:14:25.084670-06:00'
model: gpt-4-0125-preview
summary: "Een webpagina downloaden haalt de inhoud ervan van het web naar je lokale\
  \ machine. Programmeurs doen dit voor automatisering, data scraping, offline\u2026"
title: Een webpagina downloaden
---

{{< edit_this_page >}}

## Wat & Waarom?
Een webpagina downloaden haalt de inhoud ervan van het web naar je lokale machine. Programmeurs doen dit voor automatisering, data scraping, offline toegang of testen.

## Hoe te:
Hier is de snelle en vuile manier om een webpagina te downloaden met Fish Shell met het `curl` commando:

```fish
curl -O http://example.com/
```

Dit commando haalt de inhoud van de webpagina op en slaat het op met dezelfde naam als de bestandsnaam op de server (`index.html` in de meeste gevallen).

Stel nu dat je het onder een andere naam wilt opslaan:

```fish
curl -o my_page.html http://example.com/
```

Wil je zien wat je ophaalt? Zo print je het naar de console:

```fish
curl http://example.com/
```

Een voorbeelduitvoer kan er zo uitzien:

```
<!doctype html>
<html>
<head>
    <title>Voorbeeld Domein</title>
...
```

## Diepere Duik
Terug in de vroege dagen was het ophalen van webpagina's meer tovenarij op de commandolijn dan iets anders. Hulpmiddelen zoals `wget` en `curl` werden basisbenodigdheden. `curl`, al rond sinds '97, heeft de tand des tijds doorstaan voor het leveren van gegevens met URL-syntax.

Waarom `curl` boven `wget`? `curl` is meer een Zwitsers zakmes voor gegevensoverdracht, omgaand met een reeks protocollen en gegevensformaten. Hoewel beide webpagina's kunnen downloaden, kan `curl` ook gegevens uploaden, en het ondersteunt meer protocollen en wordt vaak gebruikt als een back-end tool door andere software.

Fish Shell zelf downloadt geen webpagina's; het is slechts de interface. Maar combineer het met `curl`, en je hebt een krachtige maar eenvoudige eenregels web-ophaalinstallatie.

Sommigen zouden kunnen opwerpen dat het gebruik van meer moderne hulpmiddelen zoals `httpie` of browsergebaseerde automatisering met tools zoals Selenium voor complexere taken zoals omgaan met Javascript-zware pagina's. Echter, voor de snelle en eenvoudige download, houdt `curl` nog steeds stand.

## Zie Ook

- curl projectwebsite voor meer details: [https://curl.se/](https://curl.se/)
- Voor een diepere duik in HTTP-operaties met `curl`, zie de man-pagina: `man curl`
- httpie als een gebruiksvriendelijk HTTP-client alternatief: [https://httpie.org/](https://httpie.org/)
- Fish Shell documentatie voor het afhandelen van andere shell-gerelateerde taken: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
