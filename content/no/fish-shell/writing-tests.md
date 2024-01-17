---
title:                "Skriving av tester"
html_title:           "Fish Shell: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive tester er en viktig del av programmering. Det er en måte å sørge for at koden vår fungerer som den skal og å fange eventuelle feil eller bugs før de når produksjonsstadiet. Dette sparer oss for tid og unødvendige problemer.

## Hvordan:
Å skrive tester i Fish Shell er enkelt og effektivt. Først må vi definere en funksjon med navnet "test". Dette kan gjøres ved å bruke nøkkelordet "function" etterfulgt av funksjonsnavnet og "test". Inne i denne funksjonen kan vi bruke kommandoer og sammenligne forventet og faktisk utgang ved hjelp av enkle sammenligningsoperatører. 

```Fish Shell 
function test_example
    some_command
    if test $status -eq 0
        echo "Test vellykket!"
    else 
        echo "Test feilet!"
    end
end
```

## Dypdykk:
Å skrive tester er ikke noe nytt i programmeringsverdenen. Det har vært en del av "Test Driven Development" (TDD) prinsippet siden begynnelsen av 2000-tallet. Det finnes også andre programmeringsspråk og rammeverk som tilbyr innebygde tester, som for eksempel Python og RSpec for Ruby. For å implementere tester i større prosjekter, kan man bruke et testrammeverk som for eksempel "Bats" eller "shelltestrunner" som er spesifikt laget for shell-skripting.

## Se også:
- [Fish Shell dokumentasjon om testing.](https://fishshell.com/docs/current/index.html#testing)
- [Test Driven Development på Wikipedia.](https://en.wikipedia.org/wiki/Test-driven_development)
- [Bats testrammeverk.](https://github.com/bats-core/bats-core)
- [ShellTestRunner testrammeverk.](https://github.com/bats-core/bats-core)