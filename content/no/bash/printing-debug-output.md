---
title:                "Bash: Utskrift av feilsøkingsutgang"
simple_title:         "Utskrift av feilsøkingsutgang"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å legge inn feilrettet utskrift i koden din kan virke som en ekstra trinn som tar tid og krefter, men det kan spare deg for mye hodepine og feilsøking senere. Ved å skrive ut kommander underveis i koden, kan du se hvordan verdier endres og hvilke deler av koden som blir kjørt, noe som gjør feilsøking enklere og mer effektiv.

## Hvordan lage debug utskrift

Det er enkelt å legge til utskrift i Bash-kode. Bruk kommandoen "echo" for å skrive ut variabler eller andre verdier. Du kan også bruke "printf" kommandoen for å formatere utskriften din.

```Bash
# Skriv ut en enkel beskjed
echo "Velkommen til mitt Bash-program!"

# Skriv ut verdien av en variabel
navn="Lena"
echo "Hei, mitt navn er $navn."

# Formater utskriften med printf
alder=25
printf "Jeg er %d år gammel." $alder
```

Dette gir følgende utskrift:

```Bash
Velkommen til mitt Bash-program!
Hei, mitt navn er Lena.
Jeg er 25 år gammel.
```

Du kan også bruke "if" og "else" uttrykk for å skrive ut forskjellige meldinger avhengig av en bestemt betingelse.

```Bash
if [ $alder -gt 18 ]; then
    echo "Du er myndig."
else
    echo "Du er ikke myndig."
fi
```

Dette vil skrive ut enten "Du er myndig." eller "Du er ikke myndig." avhengig av verdien av variabelen "alder".

## Dykk dypere

Det finnes flere avanserte måter å legge til feilrettet utskrift i Bash-kode, som å lage spesifikke funksjoner for utskrift, bruke "set -x" kommandoen for å aktivere sporing, eller til og med bruke et verktøy som "strace" for å spore programmets handlinger.

Det er også viktig å huske på å fjerne eller deaktivere utskrift koden når den ikke lenger trengs, slik at den ikke påvirker programmets ytelse.

Se også

- [Bash Guide for nybegynnere](http://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Debugging Bash scripts](https://linuxconfig.org/debugging-bash-script-and-shell-problems-commands-and-syntax)
- [How to add debug output in Bash scripts](https://linuxhint.com/debug_output_bash_script/)