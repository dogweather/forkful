---
date: 2024-01-26 00:55:05.937706-07:00
description: "Feilh\xE5ndtering er m\xE5ten du h\xE5ndterer ting p\xE5 n\xE5r koden\
  \ din ikke fungerer som den skal. Det er n\xF8kkelen fordi det hjelper programmene\
  \ dine \xE5 feile med\u2026"
lastmod: '2024-03-13T22:44:41.190888-06:00'
model: gpt-4-1106-preview
summary: "Feilh\xE5ndtering er m\xE5ten du h\xE5ndterer ting p\xE5 n\xE5r koden din\
  \ ikke fungerer som den skal. Det er n\xF8kkelen fordi det hjelper programmene dine\
  \ \xE5 feile med\u2026"
title: "Feilh\xE5ndtering"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Feilhåndtering er måten du håndterer ting på når koden din ikke fungerer som den skal. Det er nøkkelen fordi det hjelper programmene dine å feile med verdighet og gir brukere klare instruksjoner, i stedet for å bare krasje og brenne.

## Hvordan gjøre:

Her er den klassiske `try-catch`-blokken:

```javascript
try {
  // Kode som kan kaste en feil
  let result = potentiallyRiskyOperation();
  console.log('Suksess:', result);
} catch (error) {
  // Hva gjøre hvis en feil blir kastet
  console.error('Oisann:', error.message);
}
```

Eksempel på utskrift når ingen feil oppstår:
```
Suksess: 42
```

Og når det er en feil:
```
Oisann: Noe gikk galt
```

For asynkron kode, hvor promises er involvert, bruk `try-catch` i en `async` funksjon:

```javascript
async function fetchData() {
  try {
    let data = await fetch('https://api.example.com/data');
    console.log('Data hentet:', data);
  } catch (error) {
    console.error('Feil ved henting av data:', error.message);
  }
}

fetchData();
```

## Dypdykk

Feilhåndtering i JavaScript har utviklet seg. Tilbake i tiden (ES3, rundt 1999), hadde vi bare `try-catch`-blokken. Ikke super fleksibel, men den gjorde jobben.

ES6 (2015) introduserte Promises og ga oss `.then()` og `.catch()`, som tillater oss å håndtere asynkrone feil mer nådig.

```javascript
fetch('https://api.example.com/data')
  .then(data => console.log('Data hentet:', data))
  .catch(error => console.error('Feil ved henting av data:', error.message));
```

Når det gjelder implementasjonsdetaljer, når en feil blir kastet, skaper JavaScript-motorer et `Error`-objekt med nyttige egenskaper som `message` og `stack`. Du kan også lage tilpassede feiltyper ved å utvide `Error`-klassen – praktisk for mer komplekse apper.

Alternativer? Du kunne ignorere feilhåndtering (dårlig idé), bruke tilbakekall med feilførste parametere (hallo, Node.js-stil), eller bli mer sofistikert med biblioteker og rammeverk som tilbyr deres versjoner.

## Se også

For mer om feilhåndtering:

- MDN om try-catch: [MDN try...catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- Async/Await: [MDN async function](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function)
- En guide til Promises: [MDN Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- Opprette og kaste tilpassede feil: [MDN Error](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error)
