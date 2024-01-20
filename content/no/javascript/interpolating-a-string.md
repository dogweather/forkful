---
title:                "Interpolering av en streng"
html_title:           "Bash: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Strenginterpolering i Javascript: Hva, hvorfor og hvordan?

## Hva & Hvorfor?
Strenginterpolering i Javascript lar deg injisere variabler direkte i en streng uten å måtte stoppe og kombinere strenger og variabler. Dette gjør at koden er mer lesbar og stødig enn den ellers ville være.

## Hvordan Gjøre Det:
Her er noen eksempler på strenginterpolering i Javascript. Vi vil bruke den innebygde funksjonen "Template literals" for å gjennomføre denne oppgaven.

```Javascript
let navn = "Ola";
let hilsen = `Hei, ${navn}!`;
console.log(hilsen);  // Output: "Hei, Ola!"
``'

I det overnevnte eksempelet kombinerer vi variabelen 'navn' med en tekststreng ved hjelp av den innebygde 'Template literals' funksjonen i Javascript.

## Dypdykk
Strenginterpolering ble introdusert i Javascript som en del av ES6-standard i 2015, som respons til programmørers behov for mer behagelige måter å håndtere strenger på.

Alternativer til strenginterpolering inkluderer bruk av "+" operatør eller 'concat()' metoden for å kombinere strenger og variabler. Men disse teknikkene kan virke mer tidkrevende og mindre lesbare sammenlignet med strenginterpolering.

Implementeringen av strenginterpolering i Javascript er relativt rett frem. Den tilrettelegger bruk av '`${}`' syntaksen inne i en streng for å inkludere en variabel eller uttrykk.

## Les Mer
For å dykke dypere, kan du sjekke følgende kilder for mer informasjon og veiledning:
- [MDN Web Docs: Template literals](https://developer.mozilla.org/nb-NO/docs/Web/JavaScript/Reference/Template_literals)
- [ES6 for Everyone](https://es6.io/)
- [JavaScript.info: String interpolation](https://javascript.info/string-interpolation)