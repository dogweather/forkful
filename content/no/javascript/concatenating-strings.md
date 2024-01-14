---
title:    "Javascript: Sammenslåing av strenger"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# Hvorfor?

Å kombinere strenger, eller "konkatenering", er en viktig del av Javascript-programmering. Det lar deg lage mer dynamiske og tilpassede strenger som kan brukes til å vise informasjon eller endre teksten på en nettside.

# Hvordan gjøre det

For å konkatenering i Javascript, må du bruke operatøren "+" for å kombinere to eller flere strenger. La oss si at du har to variabler, "navn" og "by", og du vil lage en setning som sier "Hei, mitt navn er [navn] og jeg er fra [by]".

```
Javascript
var navn = "Sofie";
var by = "Oslo";

console.log("Hei, mitt navn er " + navn + " og jeg er fra " + by);
```

Dette vil produsere følgende output:

```
Hei, mitt navn er Sofie og jeg er fra Oslo
```

Du kan også konkatenering strings med andre datatyper som tall. Javascript vil konvertere tall til en string automatisk når det brukes med "+" operatøren.

# Dyp dykk

Det er viktig å merke seg at konkatenering av strenger kan føre til uønskede resultater hvis det ikke brukes riktig. For eksempel, hvis du prøver å kombinere en streng og et nummer uten å bruke "+" operatøren, vil du ende opp med et string-objekt istedenfor en ny streng.

Det finnes også alternative måter å konkatenering strenger på i Javascript, som ved hjelp av innebygde funksjoner som "concat()" og "join()". Disse alternativene kan være nyttige for mer avanserte string manipuleringer.

# Se Også

- [W3Schools - Javascript String Concatenation](https://www.w3schools.com/js/js_string_concat.asp)
- [MDN - String concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/String_concatenation)
- [JavaScript.info - Strings Concatenation](https://javascript.info/string concatenation)