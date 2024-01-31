---
title:                "Een webpagina downloaden"
date:                  2024-01-28T21:59:36.364606-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een webpagina downloaden"

category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/typescript/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een webpagina downloaden betekent het binnenhalen van de HTML, CSS en mogelijk andere bronnen van de URL die je bezoekt. Programmeurs doen dit om inhoud te verwerken, gegevens te schrapen, te controleren op updates of om websites te cachen voor offline gebruik.

## Hoe:

Je kunt een webpagina downloaden in TypeScript met behulp van Node.js en de `node-fetch` bibliotheek. Zo doe je dat:

```TypeScript
import fetch from 'node-fetch';

async function downloadWebPage(url: string): Promise<void> {
    try {
        const response = await fetch(url);
        const body = await response.text();
        console.log(body); // Dit print de HTML inhoud naar de console
    } catch (error) {
        console.error('Download mislukt:', error);
    }
}

// Gebruik de functie
downloadWebPage('https://example.com');
```

Voorbeelduitvoer (afgekapt):
```
<!doctype html>
<html>
<head>
    <title>Voorbeeld Domein</title>
...
</html>
```

## Diepgaand

Historisch gezien werd webinhoud gedownload via tools zoals `wget` of `curl` in command-line omgevingen. In moderne programmering hebben we echter bibliotheken zoals `node-fetch`, `axios`, of `request` (verouderd maar nog steeds in gebruik) die meer functionaliteit bieden en gemakkelijker te integreren zijn in onze JavaScript/TypeScript applicaties.

Bij het downloaden van een webpagina komt meer kijken dan alleen de HTML. CSS, JavaScript, afbeeldingen en andere activa maken deel uit van de deal. Meestal wordt eerst alleen de HTML binnengehaald, en dan wordt eventuele aanvullende verwerking of downloaden bepaald door wat je van de pagina nodig hebt.

Wat implementatie betreft, is `node-fetch` in wezen de window.fetch API voor Node.js. Het retourneert een belofte die oplost naar de response van het verzoek, waardoor je ofwel een tekststroom (.text()), een JSON-object (.json()), of zelfs een buffer (.buffer()) voor binaire gegevens kunt verkrijgen.

Houd er rekening mee dat rechten voor webscraping worden bepaald door het `robots.txt` bestand en de gebruiksvoorwaarden van een website. Controleer altijd of je een site mag scrapen en respecteer snelheidslimieten om juridische problemen of het krijgen van een IP-verbod te voorkomen.

## Zie Ook

- [`node-fetch` documentatie](https://github.com/node-fetch/node-fetch)
- [MDN Web Docs over Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [`axios` bibliotheek](https://github.com/axios/axios)
- [HTTP-statuscodes (om reacties te verwerken)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)
- [Legaliteit van webscraping](https://benbernardblog.com/web-scraping-and-crawling-are-perfectly-legal-right/)
