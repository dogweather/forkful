---
title:                "Skicka en http-förfrågan"
html_title:           "Javascript: Skicka en http-förfrågan"
simple_title:         "Skicka en http-förfrågan"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Skicka HTTP-förfrågan med Javascript

## Vad & Varför?
Att skicka en HTTP-förfrågan innebär att din applikation initierar en åtgärd på en annan server över webben. Detta är avgörande för front-end-utvecklare eftersom det låter dem interagera och utbyta data med webbservrar.

## Hur gör man
Användningen av `fetch()` i Javascript är ett snabbt och lätt sätt att skicka HTTP-förfrågningar. Här är ett grundläggande exempel:

```Javascript
fetch('https://api.mittwebbplats.se/data', {
  method: 'GET',
})
.then(response => response.json())
.then(data => console.log(data))
.catch((error) => {
  console.error('Error:', error);
});
```

Vid körning av koden ovan får du ett JSON-svar från 'https://api.mittwebbplats.se/data'.

## Djupdykning
`Fetch()` är den moderna lösningen för att skicka HTTP-förfrågningar och är inbyggd i de flesta moderna webbläsare. Men tidigare använde utvecklare `XMLHttpRequest`.

Ett alternativ till `fetch()` är `axios`, en tredjeparts paketleverantör med fler funktioner än `fetch()`. Axios erbjuder automatisk omvandling av JSON data, felhantering, och har stöd för äldre webbläsare.

Både `fetch()` och `axios` är verk för asynkron programmering, ett koncept i Javascript för att hantera operationer som tar tid, som nätverksförfrågningar. De returnerar ett `Promise` -objekt, en indikation på det eventuella slutförandet av en asynkron operation och dess värde.

## Se även
För vidare läsning och mer djupgående kunskap kan följande källor vara till nytta:

- [MDN-webbdokumentation för Fetch](https://developer.mozilla.org/sv-SE/docs/Web/API/Fetch_API)
- [Detaljerad guide för Axios](https://axios-http.com/docs/intro)
- [Asynkron programmering i Javascript](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous)