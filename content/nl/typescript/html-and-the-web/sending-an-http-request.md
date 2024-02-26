---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:41.249158-07:00
description: "Het verzenden van een HTTP-verzoek is hoe je programma gegevens van\
  \ een server vraagt of gegevens naar een server stuurt. Programmeurs doen dit omdat\
  \ het\u2026"
lastmod: '2024-02-25T18:49:47.893829-07:00'
model: gpt-4-0125-preview
summary: "Het verzenden van een HTTP-verzoek is hoe je programma gegevens van een\
  \ server vraagt of gegevens naar een server stuurt. Programmeurs doen dit omdat\
  \ het\u2026"
title: Een HTTP-verzoek verzenden
---

{{< edit_this_page >}}

## Wat & Waarom?

Het verzenden van een HTTP-verzoek is hoe je programma gegevens van een server vraagt of gegevens naar een server stuurt. Programmeurs doen dit omdat het de hoeksteen is van interactie met webdiensten, API's en externe bronnen.

## Hoe:

In TypeScript gebruik je doorgaans de Fetch API om HTTP-verzoeken te versturen. Hier is een snel voorbeeld, met gebruik van `async/await` voor de eenvoud:

```typescript
async function fetchData(url: string): Promise<void> {
  try {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`HTTP-fout! status: ${response.status}`);
    }
    const data = await response.json();
    console.log(data);
  } catch (error) {
    console.error('Fetch-fout:', error);
  }
}

fetchData('https://jsonplaceholder.typicode.com/todos/1');
```

Voorbeelduitvoer voor een geslaagd verzoek:

```json
{
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

## Diepere Duik

HTTP-verzoeken zijn van cruciaal belang sinds het begin van het web; ze zijn hoe browsers en servers communiceren. Voordat `fetch` er was, was er XMLHttpRequest (XHR), dat de klus klaarde maar als papierwerk aanvoelde. `fetch`, een modern alternatief, is op promises gebaseerd, schoner en maakt deel uit van het window-object in de meeste moderne browsers.

Alternatieven voor `fetch` in TypeScript omvatten bibliotheken zoals Axios, die meer functies bieden en soms gemakkelijker te hanteren zijn. Axios transformeert JSON-gegevens automatisch, handelt verzoekannulering af en biedt betere foutafhandeling.

Achter de schermen wordt TypeScript gecompileerd naar JavaScript. Wanneer je een HTTP-verzoek met `fetch` verzendt, gebruik je in wezen de native Fetch API van de browser. TypeScript's typecontrole verbetert de stabiliteit van je code door typefouten op te vangen tijdens de compilatietijd.

## Zie Ook

- MDN Webdocs over Fetch: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- Axios GitHub-repo: https://github.com/axios/axios
- Een vergelijking van HTTP-verzoekbibliotheken: https://www.npmtrends.com/axios-vs-fetch
