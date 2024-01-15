---
title:                "Kapitalisering av en streng"
html_title:           "Javascript: Kapitalisering av en streng"
simple_title:         "Kapitalisering av en streng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skal man bry seg med å gjøre om en streng (string) til store bokstaver? Vel, dette er et vanlig scenario i programmering, spesielt når det kommer til formatering av tekst. Å ha en funksjon som kan kapitalisere en streng kan spare mye tid og gjør koden vår mer effektiv.

## Hvordan gjøre det

Man kan kapitalisere en streng på flere måter i Javascript. En av de enkleste metodene er ved å bruke metoden `toUpperCase()` som finnes på alle strenger i Javascript. Her er et eksempel på hvordan man kan bruke denne metoden:

````Javascript
let tekst = "Hei, verden!";
let storTekst = tekst.toUpperCase();

console.log(storTekst); // Output: HEI, VERDEN!
````

Vi kan også kapitalisere kun den første bokstaven i en streng ved å bruke metoden `charAt()` sammen med `toUpperCase()`. Her er et eksempel:

````Javascript
let tekst = "god morgen";
let storFørsteBokstav = tekst.charAt(0).toUpperCase() + tekst.slice(1);

console.log(storFørsteBokstav); // Output: God morgen
````

En annen metode er å bruke en løkke sammen med `toUpperCase()` for å kapitalisere alle første bokstaver i ordene i en streng. Her er et eksempel på hvordan det kan gjøres:

````Javascript
let tekst = "javascript er et morsomt programmeringsspråk";
let ord = tekst.split(" ");
let nyTekst = [];

for(let i = 0; i < ord.length; i++){
    nyTekst.push(ord[i].charAt(0).toUpperCase() + ord[i].slice(1));
}

console.log(nyTekst.join(" ")); // Output: Javascript Er Et Morsomt Programmeringsspråk
````

## Dypdykk

I tillegg til metodene nevnt over, finnes det også libraries og frameworks som kan hjelpe med å kapitalisere strenger i Javascript. Et eksempel på dette er `lodash` som har en funksjon kalt `capitalize()` som gjør akkurat det vi har gjort i eksempelet over; kapitaliserer første bokstav i hvert ord i en streng.

Det er også verdt å merke seg at med Unicode blir kapitalisering av strenger ikke så rett frem som det kan virke. Språk som tysk og tyrkisk har for eksempel bokstaver som "ß" og "i" som skal ha ulik kapitalisering basert på sammenhengen de brukes i. Dette kan føre til utfordringer når man jobber med internasjonaliserte applikasjoner.

## Se også

Her er noen relevante lenker for å lære mer om å kapitalisere strenger i Javascript:

- [String.prototype.toUpperCase() - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [String.prototype.charAt() - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [Lodash - capitalize()](https://lodash.com/docs/4.17.15#capitalize)