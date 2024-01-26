---
title:                "Feilhåndtering"
date:                  2024-01-26T00:58:37.145866-07:00
model:                 gpt-4-1106-preview
simple_title:         "Feilhåndtering"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/handling-errors.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å håndtere feil handler om å forvente det uforventede; det er hvordan vi tar oss av situasjoner når ting går galt i koden vår. Vi gjør det for å unngå krasjer og for å gi brukerne en problemfri opplevelse, selv når det uforventede inntreffer.

## Hvordan:
I TypeScript innebærer feilhåndtering ofte bruk av `try`, `catch`, og `finally` blokker.

```typescript
function risikabelOperasjon() {
  throw new Error("Noe gikk galt!");
}

function håndterFeil() {
  try {
    risikabelOperasjon();
  } catch (error) {
    console.error("Fanget en feil:", error.message);
  } finally {
    console.log("Dette kjører alltid, feil eller ikke.");
  }
}

håndterFeil();
```

Eksempel på utdata:

```
Fanget en feil: Noe gikk galt!
Dette kjører alltid, feil eller ikke.
```

Asynkront eksempel med løfter (promises):

```typescript
async function asynkronRisikabelOperasjon() {
  return new Promise((resolve, reject) => {
    // Simulerer en feil
    reject("Mislyktes elendig");
  });
}

async function håndterAsynkroneFeil() {
  try {
    await asynkronRisikabelOperasjon();
  } catch (error) {
    console.error("Fanget asynkron feil:", error);
  }
}

håndterAsynkroneFeil();
```

Eksempel på utdata:

```
Fanget asynkron feil: Mislyktes elendig
```

## Dypdykk
Feilhåndtering har vært en grunnpilar i programmering siden starten. I TypeScript, som bygger på JavaScript, ble feilhåndtering mer robust med introduksjonen av async/await i ECMAScript 2017. Før det, stolte vi ofte på tilbakeringingsfunksjoner (callbacks) og løfter (promises) for å håndtere feil i asynkron kode.

Et alternativ til `try/catch` i TypeScript er å bruke feilgrenser (error boundaries) tilbudt av rammevarek (frameworks) som React. For server-side håndtering kan vi bruke mellomvare (middleware) i plattformer som Express.js for å sentralisere feilhåndtering.

Når det gjelder implementering, har ikke TypeScript sin egen feilhåndteringsmekanisme, men stoler på JavaScripts. Tilpassede feilklasser kan utvide `Error`-klassen for å tilby mer beskrivende feilinformasjon.

## Se Også
- [MDN om try/catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- [Async/Await på MDN](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Async_await)
- [Bruke Feilgrenser i React](https://reactjs.org/docs/error-boundaries.html)
- [Feilhåndtering i Express.js](https://expressjs.com/en/guide/error-handling.html)