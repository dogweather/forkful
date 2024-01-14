---
title:                "Javascript: Store bokstaver i en streng"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive kode handler ikke bare om å få programmet til å fungere, men også om å gjøre den leselig og forståelig for andre utviklere. En måte å gjøre dette på er å formatere strenger i riktig form, som å gjøre den første bokstaven i et ord til en stor bokstav. Dette gir en bedre visuell struktur og gjør det enklere å lese koden.

## Slik gjør du det

For å kapitalisere en streng, kan du bruke metoden `.toUpperCase()` i kombinasjon med metoden `.charAt()` for å få tak i den første bokstaven i strengen. Deretter kan du kombinere denne bokstaven med resten av strengen ved hjelp av metoden `.substring()`. Her er et eksempel på hvordan dette kan gjøres i Javascript:

```Javascript
let tekst = "dette er en tekst som skal kapitaliseres";

// Finner den første bokstaven og konverterer den til stor bokstav
let førsteBokstav = tekst.charAt(0).toUpperCase();

// Finner resten av strengen og konverterer den til små bokstaver
let restenAvTekst = tekst.substring(1).toLowerCase();

// Kombinerer den kapitaliserte bokstaven og resten av strengen
let kapitalisertTekst = førsteBokstav + restenAvTekst;

console.log(kapitalisertTekst); // Output: Dette er en tekst som skal kapitaliseres
```

Du kan også bruke en enklere metode som `.replace()` for å erstatte den første bokstaven i strengen med dens kapitaliserte versjon. Denne metoden tar imot en funksjon som parameter, hvor du kan manipulere og returnere den ønskede strengen etter å ha funnet den første bokstaven. Her er et eksempel på hvordan dette kan gjøres:

```Javascript
let tekst = "dette er en tekst som skal kapitaliseres";

let kapitalisertTekst = tekst.replace(tekst.charAt(0), tekst.charAt(0).toUpperCase());

console.log(kapitalisertTekst); // Output: Dette er en tekst som skal kapitaliseres
```

Det finnes også mange innebygde funksjoner og biblioteker som kan kapitalisere strenger på en enkel måte, avhengig av hvilket programmeringspråk eller bibliotek du bruker.

## Dypdykk

I tillegg til å bruke kapitalisering for å gjøre koden mer lesevennlig, kan det også være nyttig i situasjoner der du ønsker å sammenligne to strenger. Ved å kapitalisere begge strengene, vil de bli behandlet som like hvis de inneholder samme ord med samme store og små bokstaver.

Det er også viktig å være oppmerksom på at kapitalisering er forskjellig fra å endre bokstaver til store eller små. For eksempel vil en streng som allerede er helt i stor bokstav, fortsatt være i stor bokstav selv etter å ha blitt kapitalisert.

## Se også

- [String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [String.prototype.charAt()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [String.prototype.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)