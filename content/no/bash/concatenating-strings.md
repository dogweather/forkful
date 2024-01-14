---
title:                "Bash: Sammenslåing av tekststrenger"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bry seg om å kombinere strenger i Bash-programmering? Jo, fordi det er en nyttig og effektiv måte å manipulere og formatere tekst på. Ved å slå sammen flere strenger til en, kan man enkelt lage mer komplekse og tilpassede utskrifter.

## Hvordan

Det er enkelt å kombinere strenger i Bash med følgende syntaks:

```Bash
string1="Hei" 
string2="verden!" 
combined="$string1 $string2"
echo $combined
```

Dette vil gi utskriften "Hei verden!". Merk at man må bruke et dollar-tegn foran variabelnavnet når man vil ha verdien av variabelen.

Man kan også legge til ekstra tegn eller variabler i strengen, for eksempel:

```Bash
name="Ola"
greeting="Hei $name, velkommen!"
echo $greeting
```

Dette vil gi utskriften "Hei Ola, velkommen!". Man kan også kombinere strenger med tall og spesialtegn, for å lage mer komplekse utskrifter.

## Dype dykk

For de som er interessert i å lære mer om å kombinere strenger i Bash, er det viktig å huske på at det finnes forskjellige måter å gjøre det på. Her er noen tips og triks:

- Tilfeldige strenger: Ved hjelp av tilfeldig genererte tall, kan man lage lotto-nummer eller passord ved å kombinere tilfeldige tegn som a-z og 0-9.
- Løkker: Man kan kombinere strenger i en løkke, for å lage flere utskrifter med forskjellige verdier.
- Escape-tegn: Hvis man vil inkludere spesialtegn i strengen, som for eksempel "\n" for linjeskift, må man bruke et escape-tegn foran spesialtegnet for å unngå at det tolkes som en kommando.
- For å lære mer om å kombinere strenger og andre nyttige tips for Bash-programmering, se lenkene nedenfor.

## Se også

- Official GNU Bash Manual: https://www.gnu.org/software/bash/manual/
- Bash scripting cheatsheet: https://devhints.io/bash
- Bash examples for beginners: https://www.codecademy.com/learn/learn-the-command-line/articles/bash-scripting-cheat-sheet