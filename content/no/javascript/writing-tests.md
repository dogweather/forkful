---
title:                "Skriver tester"
html_title:           "Javascript: Skriver tester"
simple_title:         "Skriver tester"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Testskriving er en viktig del av programvareutvikling som hjelper programvareutviklere å sikre at koden fungerer som den skal. Ved å skrive tester, kan utviklere raskt oppdage og løse eventuelle feil eller bugs i koden sin før de blir utgitt til brukere. Dette bidrar til å forbedre kvaliteten på programvaren og øke påliteligheten.

## Slik gjør du det:

```Javascript
// Eksempel på en test som sjekker om et tall er større enn 10
function testStørreEnnTi(tall) {
  if(tall > 10) {
    console.log("Tallet er større enn 10");
  }
  else {
    console.log("Tallet er mindre enn eller lik 10");
  }
}

// Eksempel på bruk av testfunksjonen
testStørreEnnTi(15);
// Output: Tallet er større enn 10
```

## Dypdykk:

Historisk sett ble testing gjort manuelt av programmerere, noe som var tidkrevende og kunne føre til at feil ble oversett. Med innføringen av automatiserte tester, ble testprosessen mye mer effektiv og pålitelig. Alternativer til Javascript-testrammeverk inkluderer Mocha, Jest, og Jasmine. Implementering av tester innebærer å lage flere små tester som dekker forskjellige deler av koden, og disse testsene kan kjøres automatisk ved hjelp av et testrammeverk.

## Se også:

- [Mocha](https://mochajs.org/)
- [Jest](https://jestjs.io/)
- [Jasmine](https://jasmine.github.io/)