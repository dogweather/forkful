---
title:                "Att skicka en http-begäran"
html_title:           "Go: Att skicka en http-begäran"
simple_title:         "Att skicka en http-begäran"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Att skicka HTTP-förfrågan med TypeScript

## Vad & Varför?

En HTTP-förfrågan är ett sätt för program att "fråga" servrar om data. Programmerare gör detta för att hämta information från externa källor (som webbsidor eller databaser), uppdatera orsaklig data, eller för att trigga händelser.

## Hur gör man?

För att skicka en HTTP-förfrågan i TypeScript, använd `fetch` metoden. Här är ett exempel:

```TypeScript
fetch('https://api.mittexempel.se/datapunkt')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.error('Fel:', error));
```
Och här är ett exempel på utdata:

```TypeScript
{
  "id": 1,
  "name": "Exempeldata",
  "description": "Exempel på data hämtat från servern"
}
```

## Mer i detalj

HTTP-förfrågningar har funnits sedan början av webben, och används för att utbyta och manipulera data över internet. Alternativ till `fetch` inkluderar AJAX (en äldre teknik) och bibliotek som Axios.

`fetch` returnerar ett Promise som löser till ett Response-objekt. För att hantera detta korrekt, använde vi `then` metoden i vårt exempel ovan. Men i modern TypeScript kan du också använda `async/await` tillvägagångssätt för klarare, mer läsbar kod:

```TypeScript
async function fetchData() {
  try {
    const response = await fetch('https://api.mittexempel.se/datapunkt');
    const data = await response.json();
    console.log(data);
  } catch (error) {
    console.error('Fel:', error);
  }
}
```

## Se också

- [MDN-guide till Fetch API](https://developer.mozilla.org/sv-SE/docs/Web/API/Fetch_API/Using_Fetch)
- [Axios, ett populärt HTTP-klientbibliotek](https://github.com/axios/axios)
- [TypeScript's officiella hemsida](https://www.typescriptlang.org/)