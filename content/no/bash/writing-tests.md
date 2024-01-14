---
title:    "Bash: Å skrive tester"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester i Bash-programmering kan virke som en ekstra belastning og tidkrevende, men det kan være en verdifull praksis for å sikre god kodekvalitet. Ved å skrive tester kan du oppdage feil og problemer tidlig i utviklingsprosessen, noe som gjør det enklere å fikse dem før de blir et større problem.

## Slik gjør du det

For å skrive tester i Bash må du først installere et testbibliotek som f.eks. Bats. Etter installasjonen kan du skrive tester ved å bruke kommandoen "bats" og deretter navnet på testfilen. Du kan også skrive en "shebang" på toppen av testfilen for å kjøre den direkte som et Bash-skript.

```Bash
#! /usr/bin/env bats

@test "testing addition" {
  result=$(./add_numbers.sh 5 10)
  [ ${result} -eq 15 ]
}

@test "testing subtraction" {
  result=$(./subtract_numbers.sh 10 5)
  [ ${result} -eq 5 ]
}
```

I dette eksempelet tester vi to Bash-skript, "add_numbers.sh" og "subtract_numbers.sh", som henholdsvis legger til og trekker fra tall. Vi bruker "test" kommandoen til å definere en ny test og "result" variabelen til å lagre resultatet av Bash-skriptet. Deretter bruker vi Bash-syntax til å sjekke om resultatet er lik forventet verdi.

## Dypere dykk

Når du begynner å skrive tester i Bash, er det viktig å huske at det ikke finnes noen universell måte å skrive dem på. Det er mange forskjellige verktøy og metoder du kan bruke, så velg en som fungerer best for deg og dine prosjekter.

Det er også viktig å sørge for at testene dine er godt organisert og dekker alle deler av koden din. Det er fristende å bare teste de mest åpenbare delene, men det er viktig å også dekke hjørnetilfeller og uventede situasjoner.

Et annet tips er å skrive testene dine før du faktisk begynner å kode. Dette kalles "test-driven development" og kan bidra til bedre og mer gjennomtenkt kode.

## Se også

- [Bats testbibliotek](https://github.com/sstephenson/bats)
- [Bash-test eksempler](https://github.com/kward/shunit2)
- [Test-Driven Development i Bash](https://dev.to/thiagodebastos/test-driven-development-tdd-with-bash-2b4l)