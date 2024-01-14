---
title:                "Bash: Å skrive en tekstfil"
simple_title:         "Å skrive en tekstfil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil kan være en nyttig ferdighet for de som ønsker å lære Bash-programmering. Det lar deg lage og lagre tekst på en enkel og effektiv måte. Tekstfiler kan brukes til å lage skript som kan automatisere oppgaver, eller bare som et sted å lagre notater og informasjon.

## Hvordan

Det er enkelt å lage en tekstfil i Bash. Alt du trenger å gjøre er å åpne en terminal og skrive følgende kommando:

```Bash
touch min_tekstfil.txt
```

Dette vil lage en ny tekstfil med navnet "min_tekstfil.txt" i nåværende arbeidsmappe. Du kan deretter åpne filen med en tekstredigerer og begynne å skrive innholdet ditt.

For å legge til tekst i en eksisterende fil, kan du bruke "> >" operatøren som følger:

```Bash
echo "Dette er eksempeltekst" > > min_tekstfil.txt
```

Dette vil legge til teksten "Dette er eksempeltekst" i slutten av filen.

For å lese innholdet i en tekstfil, kan du bruke "cat" kommandoen:

```Bash
cat min_tekstfil.txt
```

Dette vil skrive ut innholdet i filen på skjermen.

## Dypdykk

Det finnes flere måter å lage og manipulere tekstfiler på i Bash. En annen nyttig kommando er "echo", som lar deg skrive ut tekst på skjermen og i en fil på en gang. For eksempel:

```Bash
echo "Dette er tekst som skrives ut på skjermen og i filen" > min_tekstfil.txt
```

Det finnes også kommandoer for å søke og erstatte tekst i en fil, som "grep" og "sed".

Husk å alltid bruke riktige filendelser når du lagrer tekstfiler i Bash. Ved å legge til ".txt" på slutten av filnavnet, vil filen bli registrert som en tekstfil.

## Se også

- [Introduction to Bash programming](https://www.gnu.org/software/bash/manual/html_node/Introduction.html)
- [Bash scripting tutorial](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Bash Tips and Tricks](https://catonmat.net/download/bash-history-cheat-sheet.pdf)