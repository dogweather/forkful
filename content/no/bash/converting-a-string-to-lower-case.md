---
title:    "Bash: Å konvertere en streng til små bokstaver"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange grunner til å ønske å konvertere en tekststreng til små bokstaver. Kanskje du jobber med et program som forventer at alle navn skal være på samme format, eller kanskje du ønsker å sammenligne to strenger uavhengig av store eller små bokstaver. Uansett årsak, så er det enkelt å gjøre i Bash-programmering.

## Hvordan

Den enkleste måten å konvertere en tekststreng til små bokstaver i Bash er ved å bruke kommandoen `tr`. Her er et eksempel på hvordan man kan gjøre dette:

```Bash
original_string="Hei PÅ DEG"
lowercase_string=$(echo "$original_string" | tr '[:upper:]' '[:lower:]')
echo "$lowercase_string"
```

I dette eksempelet oppretter vi først en variabel kalt `original_string` som inneholder teksten vi ønsker å konvertere. Deretter bruker vi `tr`-kommandoen til å konvertere alle store bokstaver til små bokstaver. Det gjør vi ved å bruke parameteren `[:upper:]` for å angi hvilke bokstaver vi ønsker å konvertere, og `[:lower:]` for å angi hvilken formatt vi ønsker å konvertere dem til. Til slutt lagrer vi resultatet i en ny variabel kalt `lowercase_string` og skriver den ut ved hjelp av `echo`-kommandoen.

Dersom man ønsker å bare konvertere en enkeltstreng i Bash, kan man også bruke kommandoen `tr` alene uten å lagre resultatet i en variabel. Her er et eksempel på hvordan man kan gjøre det:

```Bash
echo "Hei PÅ DEG" | tr '[:upper:]' '[:lower:]'
```

Dette vil skrive ut teksten "hei på deg" til terminalen.

## Dypdykk

I tillegg til å bruke `tr`-kommandoen, finnes det også andre måter å konvertere en tekststreng til små bokstaver i Bash. Her er to eksempler:

```Bash
# Ved hjelp av variabel subtring
original_string="Hei PÅ DEG"
lowercase_string="${original_string,,}"
echo "$lowercase_string"

# Ved hjelp av triggere
original_string="Hei PÅ DEG"
shopt -s extglob
lowercase_string="${original_string//[![:alnum:]]/_}"
echo "$lowercase_string"
```

I det første eksempelet bruker vi en variabel substring for å konvertere teksten, og i det andre eksempelet bruker vi triggere for å fjerne alle tegn som ikke er små bokstaver eller tall fra teksten. Begge disse eksemplene vil gi samme resultat som det første eksempelet med `tr`-kommandoen.

## Se også

- [Bash-programmering: Introduksjon](http://www.example.com)
- [Bash-programmering: Variabler](http://www.example.com)