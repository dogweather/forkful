---
title:                "Søke etter lengden av en streng"
html_title:           "Javascript: Søke etter lengden av en streng"
simple_title:         "Søke etter lengden av en streng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden til en streng er en grunnleggende oppgave som er essensiell for å kunne jobbe med tekstbaserte programmeringsspråk som Javascript. Ved å kunne finne lengden til en streng, kan du blant annet håndtere brukerinndata, manipulere tekst og utføre ulike operasjoner på strenger. Det er en viktig ferdighet å mestre for å kunne skrive effektiv og funksjonell kode.

## Hvordan

For å finne lengden til en streng i Javascript, kan du bruke .length-metoden. Denne metoden returnerer antall tegn i strengen, og du kan enkelt tilordne dette verdien til en variabel.

```Javascript
let streng = "Hei, dette er en tekststreng";
let lengde = streng.length;

console.log(lengde); // Output: 28
```

Du kan også bruke denne metoden på tall og andre datatyper som kan konverteres til en streng.

```Javascript
let tall = 12345;
let lengde = tall.toString().length;

console.log(lengde); // Output: 5
```

Det er viktig å huske at denne metoden regner med alle tegn, inkludert mellomrom og spesialtegn. Derfor kan det være lurt å bruke .trim()-metoden for å fjerne eventuelle mellomrom før du finner lengden på en streng.

## Dypdykk

Under panseret bruker Javascript Unicode-tegnsettet for å behandle strenger. Dette innebærer at selv om du ser et tegn som en enkel bokstav, kan det faktisk bestå av flere individuelle tegn. Derfor vil .length-metoden returnere antall individuelle tegn, ikke antall skriblede bokstaver.

I nyere versjoner av Javascript har du også tilgang til String.prototype.at()-metoden som lar deg hente et spesifikt tegn fra en streng basert på dets indeks. Dette er nyttig hvis du for eksempel trenger å behandle hvert tegn i en streng individuelt.

```Javascript
let streng = "Hei";

console.log(streng.at(0)); // Output: H
console.log(streng.at(2)); // Output: i
```

## Se også

- [Mozilla Developer Network: String.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Javascript.info: Working with strings](https://javascript.info/string)