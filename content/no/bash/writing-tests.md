---
title:                "Skriving av tester"
html_title:           "Bash: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?
Å skrive tester i Bash er en måte for programutviklere å sikre at koden deres fungerer som den skal. Tester er små programmer som kjører og sjekker om forventet atferd skjer i koden. Dette kan inkludere å teste ulike inndata og forventede resultater, samt finne og rapportere feil.

## Slik gjør du det:
Du kan skrive tester ved hjelp av Bash-skripting ved å bruke kommandoen `assert`. Dette lar deg spesifisere forventet resultat og sjekke om det stemmer overens med faktisk resultat. For eksempel:
```Bash
assert "1+1" "2" "Legger sammen to tall, forventet resultat er 2"
```
Du kan også bruke testingrammeverk som `Bats` eller `shunit2` for å organisere og kjøre flere tester samtidig.

Outputet av en test vil være enten "Success" hvis resultatet stemmer overens med forventningen, eller "Failure" hvis det ikke gjør det.

## Et dypdykk:
Å skrive tester har blitt en populær praksis i moderne programmering, spesielt innenfor metoder som TDD (Test Driven Development). Dette innebærer å skrive tester før koden, og sikrer dermed at koden har full testdekning før den implementeres.

Det finnes også andre alternativer for testing, som for eksempel å bruke testingspråk som `JUnit` eller testingrammeverk som `Selenium` for å automatisere tester for nettapplikasjoner.

Når det gjelder implementasjon, er det viktig å sørge for at testene dine er enkle og leselige, slik at de kan forstås og vedlikeholdes av andre utviklere.

## Se også:
- [Bash scripting tutorial](https://ryanstutorials.net/bash-scripting-tutorial/)
- [Bats testing framework](https://github.com/bats-core/bats-core)
- [shunit2 testing framework](https://github.com/kward/shunit2)