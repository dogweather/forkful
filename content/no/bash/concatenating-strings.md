---
title:                "Sammenslåing av strenger"
html_title:           "Bash: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere strenger i Bash kan være en nyttig teknikk når du ønsker å manipulere og formatere data. Dette kan hjelpe deg med å lage mer komplekse skript og automatisere oppgaver mer effektivt.

## Hvordan

For å kombinere to strenger i Bash, kan du bruke operatoren `+`. La oss si at du har en variabel `navn` som inneholder "Ole" og en annen variabel `alder` som inneholder "27". Ved å skrive `echo $navn+$alder` vil du få outputen "Ole+27". Dette er fordi operatoren `+` i Bash fungerer som en enkel lime sammen funksjon, derfor må du være bevisst på å inkludere mellomrom eller andre tegn om ønskelig. 

Det kan også være nyttig å bruke doble anførselstegn `"` i stedet for enkle anførselstegn `'` når du kombinerer strenger. Dette er fordi enkle anførselstegn i Bash ikke tillater variabler å bli interpretet, mens doble anførselstegn gjør det. Dette kan være spesielt nyttig når du ønsker å inkludere variabler eller andre spesielle tegn i strengen.

Et eksempel på hvordan man kan bruke dette i praksis kan være å lage en meny i et Bash script. Ved hjelp av concatenation kan du lage en variabel som inneholder hele menyen, samt bruke `echo` kommandoen for å vise den på skjermen. Her er et eksempel:

```Bash
# Definer meny
menu="1. Vis filer \n2. Vis mappestruktur \n3. Avslutt"
# Skriv ut meny
echo -e $menu
```

Outputen vil da være:

1. Vis filer
2. Vis mappestruktur
3. Avslutt

Dette er bare et enkelt eksempel på hvordan man kan bruke concatenation i praksis, men mulighetene er mange og det er viktig å eksperimentere for å finne ut hva som fungerer best i ulike situasjoner.

## Dypdykk

Ved å bruke concatenation i Bash, kan du også kombinere mer enn to strenger. Dette gjøres ved å bruke flere `+` operatorer etter hverandre. La oss si at du har variabelen `fornavn` som inneholder "Per" og variabelen `etternavn` som inneholder "Olsen", og du i tillegg ønsker å inkludere initialene "P.O." i slutten av strengen. Dette kan gjøres ved å skrive `echo $fornavn+$etternavn+P.O.` og outputen vil være "Per+Olsen+P.O.". 

En annen nyttig teknikk når du kombinerer strenger er å bruke en `=` operatør. Dette vil erstatte innholdet i variabelen med det nye resultatet av concatenation i stedet for å bare vise outputen på skjermen. Dette kan kombineres med `+` operatoren for å fortsette å bygge opp strengen. Her er et eksempel:

```Bash
# Definer fornavn
fornavn="Per"
# Legg til mellomnavn
fornavn=$fornavn+"Arne"
# Legg til etternavn
fornavn=$fornavn+"Olsen"
# Skriv ut fullt navn
echo $fornavn
```

Outputen vil da være "Per Arne Olsen", der variabelen `fornavn` har blitt overskrevet etter å ha blitt kombinert med to andre strenger.

## Se også

- [Documenting Code with Bash](https://www.linuxjournal.com/content/documenting-code-bash)
- [Bash String Concatenation](https://www.cyberciti.biz/faq/unix-linux-bash-append-text-to-file/)
- [Bash Tutorial - String concatenation](https://ryanstutorials.net/bash-scripting-tutorial/bash-variables.php)