---
title:                "Sammenslåing av strenger"
html_title:           "Arduino: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?
Strengsammensetning, eller strengkonkatenasjon, er produksjon av en ny streng ved å sette flere strenger sammen. Programmerere bruker det for å manipulere tekstdata enkelt og effektivt.

## Hvordan:
Her er noen grunnleggende eksempler for å vise hvordan strengkonkatenasjon fungerer i Javascript:

```Javascript
let streng1 = "Hei, ";
let streng2 = "verden!";
let sammensattStreng = streng1 + streng2; 
console.log(sammensattStreng); // Output: "Hei, verden!"
```

Eller bruk ES6's template literals for mer kompleks sammensetning:

```Javascript
let navn = "Ola";
let hilsen = `Hei, ${navn}!`;
console.log(hilsen); // Output: "Hei, Ola!"
```

## Dyp Dykk
Strengkonkatenasjon har vært en del av programmeringsspråk siden de tidligste dagene. Faktisk var many av de tidligste programmeringsspråkene streng-baserte språk, som COBOL.

Det er flere måter å gjøre strengkonkatenasjon på i Javascript, som vi så over. Du kan også bruke metoden `.concat()`, selv om dette er mindre vanlig i moderne kode:

```Javascript
let streng1 = "Hei, ";
let streng2 = "verden!";
let sammensattStreng = streng1.concat(streng2);
console.log(sammensattStreng); // Output: "Hei, verden!"
```

Det grunnleggende konseptet bak strengkonkatenasjon er det samme i de fleste programmeringsspråk, selv om den nøyaktige implementeringen kan variere.

## Se Også
For mer om Javascript-strenger, sjekk ut disse ressursene:
- [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String) 
- [W3Schools JavaScript String Reference](https://www.w3schools.com/jsref/jsref_obj_string.asp)