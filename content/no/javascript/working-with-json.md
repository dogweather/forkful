---
title:                "Å jobbe med json"
html_title:           "Javascript: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er interessert i webutvikling, er sjansen stor for at du har hørt om JSON. JSON står for JavaScript Object Notation og er et populært format for å lagre og overføre data. Her er noen grunner til hvorfor du bør lære å jobbe med JSON i JavaScript:

- JSON er enkelt å lese og skrive, og ligner på objektnotasjonen i JavaScript.
- Det er et lettvektsformat, noe som gjør det ideelt for å sende data over nettverket.
- Det er også støttet av de fleste moderne programmeringsspråk, noe som gjør det enkelt å integrere det i prosjekter.

## Hvordan

For å jobbe med JSON i JavaScript, trenger du ikke å installere noe ekstra. JavaScript har innebygd støtte for å parse og generere JSON-data.

For å starte, må du ha en variabel som inneholder data i en JavaScript-objektform. La oss si at vi har følgende objekt med informasjon om en bruker:

```Javascript
const bruker = {
    navn: "Maria",
    alder: 23,
    epost: "maria@gmail.com"
}
```

For å konvertere dette objektet til JSON-format, bruker vi metoden `JSON.stringify()`:

```Javascript
const brukerJSON = JSON.stringify(bruker);
console.log(brukerJSON);
// Output: {"navn":"Maria","alder":23,"epost":"maria@gmail.com"}
```

Som du kan se, er det resulterende JSON-objektet en streng som følger samme struktur som JavaScript-objektet. Dette gjør det enkelt å lese og håndtere.

Hvis du har en JSON-streng og ønsker å konvertere den til et JavaScript-objekt, bruker du metoden `JSON.parse()`:

```Javascript
const brukerObjekt = JSON.parse(brukerJSON);
console.log(brukerObjekt.navn);
// Output: "Maria"
```

## Dypdykk

En av de største fordelene med å bruke JSON i JavaScript er at det er enkelt å integrere med API-er. Mange av de populære API-ene som Facebook, Twitter og Google bruker JSON som standardformat for å sende og motta data.

I tillegg kan du også bruke JSON til å lagre data lokalt i nettleseren ved hjelp av Web Storage API. Dette lar deg lagre og hente brukerdata uten å måtte gjøre en serverforespørsel hver gang.

Noen andre teknikker du kan utforske for å jobbe med JSON i JavaScript inkluderer:

- Bruk `fetch()` for å hente data fra en API og konvertere det til JSON.
- Bruk `Object.keys()` og `Object.values()` for å hente nøkler og verdier fra et JSON-objekt.
- Bruk `typeof` for å sjekke om en variabel er et JSON-objekt.

## Se også

- [JSON Dokumentasjon](https://www.json.org/json-en.html)
- [MDN Web Docs om JSON](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- [JSON-filformatet](https://www.fileformat.info/format/json/)