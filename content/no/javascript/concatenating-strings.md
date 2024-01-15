---
title:                "Sammenføying av tekststrenger"
html_title:           "Javascript: Sammenføying av tekststrenger"
simple_title:         "Sammenføying av tekststrenger"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

#Hvorfor

Å kombinere eller "sammensette" strenger er en vanlig operasjon i Javascript og er nyttig for å lage dynamiske tekster og uttrykk. For eksempel kan dette være nyttig når man ønsker å vise brukerspesifikk informasjon på en nettside eller bygge en kompleks tekststreng som skal brukes i en e-post.

#Slik gjør du det

```Javascript
let navn = "Sara";
let alder = 27;

let beskjed = "Hei, mitt navn er " + navn + " og jeg er " + alder + " år gammel.";

console.log(beskjed);

//Output: "Hei, mitt navn er Sara og jeg er 27 år gammel."
```

Her har vi brukt tilleggsoperatoren "+" for å kombinere variabler og tekststrenger. Denne metoden kan også brukes for å legge til mellomrom eller andre tegn mellom strenger:

```Javascript
let fornavn = "Ole";
let etternavn = "Nordmann";

let navn = fornavn + " " + etternavn;

console.log(navn);

//Output: "Ole Nordmann"
```

En annen måte å kombineere strenger på er å bruke metoden `concat()`:

```Javascript
let setning1 = "Hei, mitt navn er ";
let setning2= "Sara";
let setning3 = " og jeg kommer fra Oslo.";

let beskjed = setning1.concat(setning2, setning3);

console.log(beskjed);

//Output: "Hei, mitt navn er Sara og jeg kommer fra Oslo."
```

#Dypdykk

Når du kombinerer strenger i Javascript, konverteres alle variabler automatisk til tekststrenger. Dette betyr at både tall og booleanske verdier vil bli behandlet som tekst. Du kan også kombinere flere strenger ved å bruke flere `concat()` funksjoner, for eksempel `string1.concat(string2).concat(string3)`.

Man kan også bruke template literals, som er et mer moderne og fleksibelt alternativ for å kombinere strenger. Det gjør man ved å bruke "backticks" (`) istedenfor anførselstegn og deretter inkludere variabler eller funksjoner ved hjelp av `${}`:

```Javascript
let navn = "Sara";
let alder = 27;

let beskjed = `Hei, mitt navn er ${navn} og jeg er ${alder} år gammel.`;

console.log(beskjed);

//Output: "Hei, mitt navn er Sara og jeg er 27 år gammel."
```

En annen nyttig ting å huske på er at når man kombinerer strenger, er det viktig å ha riktig syntaks og ikke glemme mellomrom og andre tegn. Ellers kan man få uventede resultater eller feilmeldinger.

#Se også

- [W3Schools - String Concatenation](https://www.w3schools.com/js/js_string_concat.asp)
- [MDN Web Docs - Template literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)