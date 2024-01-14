---
title:    "Fish Shell: Å finne lengden av en streng."
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

#Hvorfor
Å finne lengden på en streng er en grunnleggende funksjon som er nødvendig i mange programmeringssituasjoner. Ved å vite lengden på en streng, kan du kontrollere at teksten er innenfor en viss grense, og også utføre spesifikke operasjoner basert på lengden.

#Slik Gjør Du
```Fish Shell
set tekst "Hei, dette er en teststreng." #Definerer en variabel med en streng
echo (count $tekst) #Bruker "count" for å finne lengden på strengen og skrive ut resultatet
```
Output:
23

Det første trinnet er å definere en variabel som inneholder strengen vi vil finne lengden på. Dette kan gjøres ved å bruke "set" kommandoen og gi variabelen et navn, for eksempel "tekst".

Neste steg er å bruke "count" kommandoen, som er en Fish Shell intern kommando som returnerer lengden på en streng. Ved å bruke parenteser rundt kommandoen, vil resultatet bli skrevet ut på skjermen ved hjelp av "echo" kommandoen.

#Dypdykk
Når vi bruker "count" kommandoen, blir den interne funksjonen "string length" kjørt. Denne funksjonen tar inn en streng som input og returnerer et heltall som representerer lengden på strengen.

En ting å merke seg er at "count" kommandoen vil ignorere eventuelle escape karakterer i strengen, men vil telle nye linjer som ett tegn.

#Se Også
- [Fish Shell dokumentasjon om count](https://fishshell.com/docs/current/cmds/count.html)
- [Intern "string length" funksjon dokumentasjon](https://fishshell.com/docs/current/index.html#string-length)