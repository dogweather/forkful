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

## Hvorfor

Så du har begynt å bruke Fish Shell i terminalen din, og lurer kanskje på hvorfor du bør bruke tid på å skrive tester for koden din? Vel, å skrive tester er en god måte å sikre at koden din fungerer som den skal og å identifisere eventuelle feil eller bugs i koden tidlig, noe som kan spare deg for mye frustrasjon på lang sikt.

## Slik gjør du det

For å skrive tester i Fish Shell, må du først opprette en testfil. Dette kan gjøres ved å åpne en tekstredigerer og skrive følgende kode:

```Fish Shell
# Det første du må gjøre er å inkludere test-rammeverket:
source /usr/local/share/fish/tools/test.fish

# Bruk "begin" og "end" for å definere en gruppe av tester:
begin gruppe_navn

# Skriv testene dine ved å bruke "test" kommandoen:
test "1 er lik 1" -a 1 = 1

# Avslutt gruppen med "end":
end

# Kjør testene ved å kjøre filen med test-rammeverket:
fish test_filen.fish
```

Etter å ha kjørt filen din, vil du få utskrift som viser hvilke tester som besto og hvilke som feilet. Hvis alle testene bestod, har du skrevet fungerende kode!

## Grav dypere

Nå som du vet hvordan du skriver og kjører tester i Fish Shell, kan du begynne å utforske de forskjellige funksjonene og mulighetene som test-rammeverket gir deg. For eksempel kan du bruke "test_not" for å sjekke at en betingelse ikke er sann, eller "test_equal" for å sammenligne verdier som ikke er like. Det er også mulig å håndtere unntak og feilmeldinger ved hjelp av "catch" kommandoen. Ved å utforske disse mulighetene og eksperimentere med kode, kan du bli mer komfortabel med å skrive tester og skrive mer omfattende og effektive tester for koden din.

## Se også

- [Fish Shell Test Rammeverk Dokumentasjon](https://fishshell.com/docs/current/cmds/test.html)
- [En Introduksjon til Test-Driven Utvikling i Fish Shell](https://medium.com/fish-tips/easy-test-driven-development-tdd-in-fish-shell-50322c0402d7)
- [Å Skrive Klarere Kode ved å Skrive Tester i Fish Shell](https://stackoverflow.blog/2019/07/18/write-more-understandable-code-by-writing-tests/)