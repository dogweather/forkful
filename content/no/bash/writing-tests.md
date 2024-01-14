---
title:                "Bash: Skriver tester"
simple_title:         "Skriver tester"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av å være en god Bash-programmerer. Med tester kan du sikre at koden din gjør det den skal, og at eventuelle endringer ikke fører til uønskede resultater. Tester gir også en dokumentasjon av koden din som andre utviklere kan se på og forstå raskt. Det kan også være en fin måte å organisere og strukturere koden din på.

## Hvordan gjøre det

For å skrive tester i Bash, kan du bruke kommandoen `test` eller `[`. Disse kommandoene sammenlikner en variabel eller et uttrykk med en forventet verdi, og returnerer sann eller usann.

Et enkelt eksempel på en test kan være å sjekke om en variabel er lik en spesifikk verdi:

```Bash 
var="hello"
if [ "$var" == "hello" ]; then
  echo "Variabelen er lik 'hello'"
else
  echo "Variabelen er ikke lik 'hello'"
fi
```

Dette vil gi ut følgende resultat:

```Bash 
Variabelen er lik 'hello'
```

Du kan også bruke tester til å sjekke om en fil eksisterer, eller om den inneholder spesifikke data. For eksempel:

```Bash 
if [ -f "fil.txt" ]; then
  echo "Filen eksisterer"
else
  echo "Filen eksisterer ikke"
fi
```

Dette vil sjekke om filen "fil.txt" eksisterer i samme mappe som skriptet, og gi ut en melding basert på resultatet.

## Dypdykk

Å skrive tester i Bash kan virke enkelt, men det er viktig å tenke på at de må være presise og dekke ulike situasjoner. Det kan være lurt å dele testene dine opp i mindre grupper basert på forskjellig funksjonalitet eller deler av koden. Det kan også være lurt å bruke variabler for å gjøre testene mer fleksible og enklere å vedlikeholde.

Du kan også bruke konstruksjoner som `if`, `elif` og `else` for å lage mer komplekse tester og håndtere ulike scenarioer. Det kan også være nyttig å bruke `-eq`, `-gt`, `-lt` og andre sammenlikningsoperatører for å sjekke tallbaserte verdier.

## Se også

- [The Bash Manual](https://www.gnu.org/software/bash/manual/) - offisiell dokumentasjon for Bash
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/) - en nybegynnerguide til Bash-programmering
- [Bash Shell Scripting Tutorial](https://bash.cyberciti.biz/guide/Main_Page) - en omfattende guide til Bash-skripting