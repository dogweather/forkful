---
title:    "Javascript: Finne lengden til en streng"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor
I JavaScript, er en streng en sekvens av karakterer som brukes til å representere tekst. Det finnes ulike scenarioer der det kan være nyttig å finne lengden av en streng, for eksempel når man ønsker å sjekke om en brukers input er for lang eller for kort, eller for å manipulere tekst på en bestemt måte. 

# Slik gjør du det
For å finne lengden av en streng i JavaScript, kan du bruke `length` egenskapen. Denne egenskapen returnerer antall karakterer i strengen og kan anvendes på både enkeltstående skjert og variabler som inneholder en streng.
    
```Javascript
let string = "Hei, dette er en bloggpost";
console.log(string.length);
// Output: 27
```
```Javascript
let brukerInput = prompt("Skriv inn din personlige melding:");
if (brukerInput.length > 50) {
    console.log("Meldingen din er for lang, vennligst kutt den ned til 50 karakterer");
} else {
    console.log("Meldingen din er lagret!");
}
```
I det første eksempelet bruker vi `length` egenskapen på variabelen `string` for å finne lengden av strengen som er lagret i den. I det andre eksempelet bruker vi `length` egenskapen til å sjekke om brukerens input er for lang og gir en tilbakemelding basert på det. 

# Dypdykk
Når vi bruker `length` egenskapen, må vi være oppmerksomme på at den kun returnerer antall *karakterer*, ikke antall *ord*. Dette kan være viktig å huske på hvis man ønsker å begrense teksten basert på et visst antall ord i stedet for antall karakterer. Det er også viktig å være klar over at blanke mellomrom og spesielle tegn også telles med i lengden av en streng.

# Se også
- [String.prototype.length - JavaScript MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [JavaScript String Length - W3Schools](https://www.w3schools.com/jsref/prop_string_length.asp)
- [Understanding String Length - SitePoint](https://www.sitepoint.com/understanding-string-length-javascript/)