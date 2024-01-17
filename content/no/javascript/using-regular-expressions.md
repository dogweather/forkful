---
title:                "Å bruke regulære uttrykk"
html_title:           "Javascript: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Regulære uttrykk er et verktøy som brukes av programmører for å søke og manipulere tekst på en effektiv måte. Det gjør det enklere å finne spesifikke strenger av tegn eller ord i en større tekst. Dette er nyttig når man for eksempel ønsker å finne spesifikke mønstre i en tekst, som for eksempel e-postadresser eller telefonnumre.

## Hvordan:

 ```Javascript
let text = "Heisenberg, Walter and Jesse have been working together to make meth.";

let pattern = /Jesse/; // regex pattern to match the string "Jesse"

console.log(pattern.test(text)); // output: true
 ```

I dette eksempelet bruker vi et regulært uttrykk (`/Jesse/`) for å finne om teksten inneholder strengen "Jesse". Vi bruker også metoden `.test()` for å teste om uttrykket matcher med teksten. I dette tilfellet vil uttrykket matche og derfor vil `.test()` returnere `true`.

## Dypdykk:

Regulære uttrykk har eksistert siden 1950-tallet og har blitt en viktig del av programmering siden da. Alternativene til å bruke regulære uttrykk inkluderer å bruke innebygde funksjoner som `indexOf()` eller `includes()` for å søke etter enkel strenger. Men disse kan være begrensende når man ønsker å finne mer komplekse mønstre.

For å bruke regulære uttrykk i Javascript, må du bruke `RegExp` konstruktøren og et set av flagg for å endre oppførselen. Det finnes også flere nettsider og programmer som kan hjelpe deg med å lage og teste regulære uttrykk.

## Se også:

- [MDN - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [W3Schools - Regular Expressions](https://www.w3schools.com/js/js_regexp.asp)
- [Regex101 - Online Regex Tester and Debugger](https://regex101.com/)