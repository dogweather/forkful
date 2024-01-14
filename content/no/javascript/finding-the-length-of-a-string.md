---
title:                "Javascript: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er interessert i å lære JavaScript, er det viktig å forstå grunnleggende konsepter som å finne lengden på en streng. Å kunne finne lengden på en streng kan være nyttig når du jobber med forskjellige tekstmanipulasjonsoppgaver, som å sortere og filtrere data.

## Hvordan

Det er flere måter å finne lengden på en streng i JavaScript. En måte er å bruke den innebygde `.length`-metoden. Denne metoden returnerer antall tegn i en streng.

```Javascript
let streng = "Hei, verden!";
console.log(streng.length);
// Output: 13
```

Du kan også bruke en løkke, for eksempel en `for`-løkke, for å iterere gjennom strengen og telle antall tegn.

```Javascript
let streng = "Javascript er gøy!";
let teller = 0;

for (let i = 0; i < streng.length; i++) {
    teller++;
}

console.log(teller);
// Output: 17
```

En annen nyttig metode er `.trim()`, som fjerner mellomrom fra begynnelsen og slutten av en streng. Dette kan være nyttig hvis du ønsker å finne lengden på en streng uten å inkludere mellomrom.

```Javascript
let streng = "   Javascript er gøy!   ";
console.log(streng.trim().length);
// Output: 17
```

## Dypdykk

Det er viktig å forstå at lengden på en streng i JavaScript er antall tegn, ikke antall ord. Dette kan føre til forvirring hvis strengen inneholder mellomrom eller spesialtegn. I tillegg kan Unicode-tegn, som emoji, telle som flere tegn og påvirke lengden på en streng.

En annen ting å merke seg er at `.length`-metoden returnerer et heltall, ikke en faktisk verdi. Dette betyr at det ikke er mulig å finne lengden på en streng med desimaler.

## Se også

- [W3 Schools - JavaScript String Length](https://www.w3schools.com/jsref/prop_string_length.asp)
- [MDN - String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)