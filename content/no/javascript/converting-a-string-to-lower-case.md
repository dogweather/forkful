---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å konvertere en streng til små bokstaver er når vi endrer alle tegn i en tekststreng til små bokstaver. Programmerere gjør dette for å normalisere data og forbedre søkets nøyaktighet.

## Hvordan gjør man det:

Enkelt! Bruk `toLowerCase()` metode på strengen. Her er et eksempel:

```Javascript 
var tekst = "Hei Verden!";
var lav = tekst.toLowerCase();
console.log(lav);  // Resultat: "hei verden!"
```

Mer kompleks? Bruk en løkke for å gjøre det på hele array:

```Javascript 
var tekstArray = ["Hei", "Verden", "!"];
var lavArray = tekstArray.map(function(tekst) { 
  return tekst.toLowerCase();
});
console.log(lavArray);  // Resultat: ["hei", "verden", "!"]
```

## Dyp Dykk

1. Historisk kontekst: `toLowerCase()` metoden har vært en del av ECMAScript (offisiell navn på JavaScript) standarden siden versjon 1 i 1997.

2. Alternativer: Hvis du trenger å håndtere internasjonale tegnsett, kan du vurdere å bruke `toLocaleLowerCase()`. Det tar systemets lokalinnstillinger i betraktning når den konverterer bokstavene.

```Javascript 
var tekst = "Γεια σου Κόσμε"; // gresk
console.log(tekst.toLocaleLowerCase()); // resultat: "γεια σου κόσμε"
```

3. Implementeringsdetaljer: Metoder `toLowerCase()` og `toLocaleLowerCase()` påvirker ikke den opprinnelige strengen. De returnerer en ny streng.

## Se Også

- MDN Web Docs, [String.prototype.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)

- MDN Web Docs, [String.prototype.toLocaleLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)

- ECMAScript 2015 [spesifikasjon](https://www.ecma-international.org/ecma-262/6.0/#sec-string.prototype.tolowercase)