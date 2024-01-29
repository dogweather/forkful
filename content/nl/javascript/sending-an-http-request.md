---
title:                "Een HTTP-verzoek verzenden"
date:                  2024-01-28T22:07:34.109744-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/javascript/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het verzenden van een HTTP-verzoek is hoe je JavaScript-code met een server communiceert. Dit wordt gedaan om gegevens uit te wisselen, bronnen op te halen of gegevens naar de server te sturen voor verwerking.

## Hoe:

JavaScript gebruikt de `fetch` API voor het verzenden van verzoeken. Hier is hoe je een eenvoudig GET-verzoek doet:

```javascript
fetch('https://jsonplaceholder.typicode.com/posts/1')
  .then(response => response.json())
  .then(json => console.log(json))
  .catch(err => console.error('Fout:', err));
```

De uitvoer zal JSON-gegevens van de URL zijn. Makkelijk, toch?

En voor een POST-verzoek:

```javascript
fetch('https://jsonplaceholder.typicode.com/posts', {
  methode: 'POST',
  lichaam: JSON.stringify({
    titel: 'foo',
    lichaam: 'bar',
    gebruikerId: 1,
  }),
  koppen: {
    'Inhoudstype': 'application/json; charset=UTF-8',
  },
})
  .then(response => response.json())
  .then(json => console.log(json))
  .catch(err => console.error('Fout:', err));
```

Dit stuurt nieuwe gegevens en geeft de reactie van de server weer.

## Diepe Duik

HTTP-verzoeken bestaan al sinds het begin van het web—denk aan HTML-formulieren. XMLHttpRequest (XHR) was ooit de standaardmethode voor het verzenden van verzoeken in JavaScript, maar het is omslachtig.

Enter `fetch`, een moderne aanpak die op promises is gebaseerd, waardoor het schoner en robuuster is. In tegenstelling tot XHR, behandelt `fetch` zowel verzoeken als reacties in een enkele, uniforme API en is ingebouwd in de taal, geen bibliotheken vereist.

Alternatieven? Zeker. Bibliotheken zoals Axios of jQuery's Ajax worden nog steeds gebruikt. Ze bieden wat syntactische suiker en oplossingen voor specifieke eigenaardigheden, maar `fetch` is native en over het algemeen de weg vooruit.

Implementatiedetails? Vergeet niet om fouten te verwerken, te werken met verschillende responstypes en je bewust te zijn van cross-origin resource sharing (CORS) regels.

## Zie Ook

- MDN `fetch` API Documentatie: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- Het gebruik van promises in JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises
- Leer over CORS: https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS
