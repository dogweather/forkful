---
title:    "Javascript: Søk og erstatt tekst"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å bytte ut tekster i koden din kan være en nyttig ferdighet å ha som en Javascript-programmerer. Det lar deg enkelt endre flere forekomster av en gitt tekst på en gang, noe som kan spare deg mye tid og krefter.

## Hvordan gjøre det

Det er flere måter å søke og erstatte tekst i Javascript. En av de mest effektive måtene er å bruke en regex (regular expression) sammen med en innebygd metode kalt `replace()`. Her er et eksempel på hvordan du kan bruke dette:

```Javascript
// Lager en ny string som inneholder tekster vi vil bytte ut
let tekst = "Hei, mitt navn er Javascript. Javascript er kjempegøy!"

// Bruker regex og replace() metoden for å endre tekst
let nyTekst = tekst.replace(/Javascript/g, "Python")

console.log(nyTekst) // Output: Hei, mitt navn er Python. Python er kjempegøy!
```

Som du kan se i eksempelet, bruker vi `/Javascript/g` som et regex-mønster og `replace()`-metoden for å bytte ut alle forekomster av "Javascript" med "Python". Det `g` etter skråstreken indikerer at vi vil at det skal endre alle forekomster, ikke bare den første.

Du kan også bruke `replace()` for å erstatte tekster med variabler eller funksjoner. Her er et annet eksempel:

```Javascript
// En variabel med tekst vi vil bruke for å bytte ut
let byttetTekst = "for å gi liv til kode!"

// Funksjon som bytter ut tekster og legger til et '!' på slutten
function erstatteTekst(match) {
  return match + byttetTekst + '!';
}

// Bruker funksjonen og replace() for å endre tekst
let nyTekst = "Vi koder ",replace('for', erstatteTekst)

console.log(nyTekst) // Output: Vi koder for å gi liv til kode!!
```

## Dypdykk

Det er også andre metoder for å søke og erstatte tekster i Javascript, som `indexOf()` og `substr()`. `indexOf()` returnerer posisjonen til en tekst i en streng, mens `substr()` returnerer en del av en streng basert på en startindeks og en lengde. Disse metodene kan også brukes på et array av strenger.

Det er viktig å merke seg at regular expressions kan være komplekse og krever litt øvelse for å mestre. Men å kunne bruke dem for å søke og erstatte tekster i kode kan være en kraftig verktøy som kan spare deg for mye tid og arbeid.

## Se også

- [MDN Web Docs - String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Web Docs - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [W3Schools - JavaScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)