---
title:    "Bash: Skriver tester"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av å utvikle gode bash programmer. Å teste koden din vil bidra til å identifisere feil tidlig og sikre at programmet fungerer som forventet. Ved å skrive tester kan du også enkelt gjøre endringer i koden din uten å bekymre deg for å ødelegge funksjonaliteten.

## Hvordan

For å skrive tester i Bash, kan du bruke test-kommandoen som lar deg sammenligne verdier og returnere en sann eller usann verdi avhengig av resultatet. La oss se på et eksempel:

```Bash
# Definerer en variabel
navn="Maria"

# Sjekker om variabelen er lik "Maria"
if [ $navn == "Maria" ];
then
    echo "Hei Maria!"
else
    echo "Hvem er du?"
fi
```

I dette eksempelet bruker vi test-kommandoen `[ $navn == "Maria" ]` for å sammenligne verdien av variabelen "navn" med strengen "Maria". Hvis betingelsen er sann, vil programmet skrive ut "Hei Maria!", ellers vil det skrive ut "Hvem er du?". Dette viser hvordan testing kan hjelpe deg med å sikre at koden din fungerer som forventet.

## Dypdykk

Nå som du har fått en introduksjon til hvordan du kan skrive tester i Bash, kan det være nyttig å vite noen flere tips og triks for å skrive effektive tester. En viktig ting å huske på er å skrive tester for alle mulige utfall, ikke bare det som forventes. Dette vil sikre at programmet ditt håndterer uventede feil eller inndata også.

Du kan også lage tester for funksjonskall ved å lese output fra en funksjon og sammenligne det med det forventede resultatet, eller ved å teste forhåndsdefinerte variabler som skal oppdateres av funksjonen. Det kan også være lurt å bruke beskrivende tester, slik at du enkelt kan forstå hva testen skal gjøre uten å måtte lese selve koden.

## Se Også

- [Bash scripting tutorial (en)](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Bash test-kommando dokumentasjon (en)](https://www.gnu.org/software/bash/manual/html_node/Bourne-Shell-Builtins.html#Bourne-Shell-Builtins)
- [Eksempler på Bash testing (en)](https://www.lifewire.com/test-for-an-empty-value-2201046)