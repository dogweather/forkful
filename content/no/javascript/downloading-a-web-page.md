---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å laste ned en webside er prosessen hvor data fra en nettressurs overføres til en lokal enhet. Programmerere tar ofte denne steget for å analysere nettdata, holde en lokal kopi, eller integrere webinnhold i sine applikasjoner.

## Hvordan:

Du kan bruke ulike biblioteker for å laste ned websider i JavaScript, men vi skal fokusere på bruk av den innebygde `fetch` funksjonen. La oss se på et eksempel:

```Javascript
fetch('https://www.dittsite.no')
  .then(response => response.text())
  .then(data => console.log(data))
  .catch(error => console.error('En feil oppstod:', error));
```

Når du kjører dette i nettleserens konsoll, bør du se HTML-koden for siden i konsollen.

## Dyp Dykk

'Fetch API', som vi nettopp brukte, er en moderne, lovende løsning for å lage HTTP-forespørsler. Den ble introdusert som en del av HTML5 og er designet for å erstatte den eldre 'XMLHttpRequest'. Den er kraftig og fleksibel, men ikke alle nettlesere støtter det ennå.

Alternativt kan du bruke 'Node.js' med 'axios' biblioteket. Denne metoden krever mer oppsett, men gir deg større kontroll og mer kraftige funksjoner.

Når det kommer til implementasjonsdetaljer, kan det å laste ned en webside være enkelt, som vist i koden over, men også mer kompleks. Du kan trenge å håndtere redirects, sertifikater, cookies, og tidsavbrudd.

## Se Også

For mer informasjon, sjekk ut disse superbegrensningene:

- 'Fetch API' dokumentasjon: [https://developer.mozilla.org/no/docs/Web/API/Fetch_API](https://developer.mozilla.org/no/docs/Web/API/Fetch_API)

- 'axios' bibliotek dokumentasjon: [https://axios-http.com/](https://axios-http.com/)

- MDN veiledning på 'Web scraping' med Node.js: [https://developer.mozilla.org/no/docs/Learn/JavaScript/Client-side_web_APIs/Fetch](https://developer.mozilla.org/no/docs/Learn/JavaScript/Client-side_web_APIs/Fetch)

God kodepraksis!