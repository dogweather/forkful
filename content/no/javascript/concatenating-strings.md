---
title:                "Javascript: Sammenføyning av strenger"
simple_title:         "Sammenføyning av strenger"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere eller "konkatenere" strenger er en grunnleggende og viktig del av å programmere i Javascript. Det er nyttig for å lage dynamiske tekster og meldinger som kan endres basert på ulike faktorer, og er et vanlig verktøy for å manipulere og behandle data.

## Hvordan

For å konkatenere strenger, kan du bruke "+" operatøren eller String.concat() metoden i Javascript. Her er et enkelt eksempel:

```javascript
var navn = "Sara";
var alder = 27;
var melding = navn + " er " + alder + " år gammel.";
console.log(melding);
```

Dette vil gi følgende output: "Sara er 27 år gammel."

En annen måte å konkatenere strenger på er å bruke "```=```" tegnet og tilordne en ny verdi til en eksisterende variabel, som vist i eksempelet under:

```javascript
var hello = "Hei";
hello = hello + ", hvordan går det?";
console.log(hello);
```

Dette vil skrive ut følgende: "Hei, hvordan går det?"

Du kan også bruke String.concat() metoden, som tar inn to eller flere argumenter og konkatenere dem sammen. Her er et eksempel:

```javascript
var adjektiv = "fin";
var substantiv = "dag";
var melding = "Hvilken " + adjektiv.concat(substantiv) + " det er i dag!";
console.log(melding);
```

Dette vil skrive ut: "Hvilken fint dag det er i dag!"

Husk at når du konkatenere tall og strenger sammen, vil tallet bli konvertert til en streng. Så "1" + "2" = "12", ikke "3".

## Deep Dive

Når du konkatenere strenger, er det viktig å huske på å legge til mellomrom og punktum på riktig sted for å få en sammenhengende tekst. Du kan også bruke escape-sekvenser, som "```/n```" for å legge til linjeskift, "```/t```" for å legge til et tabulator, eller "```/```" for å ignorere et spesielt tegn.

Det er også viktig å bruke variabler på riktig måte når du konkatenere strenger. For eksempel, hvis du bruker en variabel i et argument for String.concat() metoden, må du sikre deg at variabelen faktisk er en streng, ellers vil du få en feilmelding.

## Se Også

- [MDN Web Docs: Konkatenere strenger i Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/String_Operators#Concatenation)
- [W3Schools: String Concatenation i Javascript](https://www.w3schools.com/jsref/jsref_concat_string.asp)