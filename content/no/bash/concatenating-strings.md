---
title:                "Bash: Sammenføying av strenger"
simple_title:         "Sammenføying av strenger"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å slå sammen strenger er en viktig ferdighet å ha i Bash-programmering. Dette lar deg kombinere flere tekststrenger for å lage en lengre streng eller en variabel. Det kan være nyttig når du ønsker å lage dynamiske utskrifter eller sende data til andre kommandoer.

## Hvordan gjøre det

For å slå sammen strenger i Bash, bruker du operatoren `+` eller tilde (`~`). La oss ta en titt på et eksempel:

```Bash
# Slå sammen strenger med +
name="John"
greeting="Hei, mitt navn er " + $name

# Slå sammen strenger med ~
age=25
intro="Jeg er $age år gammel"
full_intro="Hei, " ~ $greeting ~ ", " ~ $intro

echo $full_intro # Utskrift: Hei, mitt navn er John, Jeg er 25 år gammel
```

I dette eksemplet bruker vi `+` til å slå sammen to tekststrenger og tilde (`~`) for å legge til en annen tekststreng i en variabel. Det er viktig å merke seg at det ikke skal være mellomrom rundt operatørene.

## Dypdykk

Det er også mulig å slå sammen strenger med `printf` kommandoen. Dette gir mer kontroll over formateringen av utskriften. La oss se på et eksempel:

```Bash
name="Lisa"
age=28
printf "Hei, mitt navn er %s og jeg er %d år gammel." $name $age # Utskrift: Hei, mitt navn er Lisa og jeg er 28 år gammel.
```

Her bruker vi `%s` og `%d` for å angi at variablene `$name` og `$age` skal legges til i strengen, henholdsvis som en tekst og et tall. Dette tillater også formatering av utskriften, for eksempel ved å angi antall desimaler for et tall eller justering av tekst til venstre/høyre.

## Se også

- [Bash-kommandolinjens dokumentasjon om strengmanipulasjon](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [En guide til Bash scripting](https://www.tecmint.com/bash-string-manipulation/)
- [En interaktiv Bash leksjon om strenger og variabler](https://www.learnshell.org/en/Strings_and_Variables)