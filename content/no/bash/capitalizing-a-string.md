---
title:                "Bash: Stor bokstaver i en tekststreng"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Hvorfor
Velkommen til min blogg om Bash-programmering! I dette innlegget skal vi snakke om hvordan du kan kapitalisere en streng i Bash. Dette kan være nyttig hvis du for eksempel ønsker å endre utseendet på visse ord i en tekstfil eller for å sikre konsistent formatering i en variabel.

##Slik gjør du det
For å kapitalisere en streng i Bash, kan du bruke kommandoen "tr", som står for "translate". Dette er en kommando som utfører en enkel substitusjon i en tekstfil. Her er et eksempel på hvordan du kan bruke det til å kapitalisere en streng:

```Bash
tekst="dette er et eksempel"
kapitalisert=$(echo $tekst | tr '[:lower:]' '[:upper:]')
echo $kapitalisert
```

Dette vil returnere "DETTE ER ET EKSEMPEL" som output. La oss ta en nærmere titt på hva som skjer her. Først lagrer vi teksten vi vil kapitalisere i en variabel kalt "tekst". Deretter bruker vi kommandoen "tr" og spesifiserer to sett med tegn som vi ønsker å bytte ut. I dette tilfellet bruker vi "[:lower:]" for å representere alle små bokstaver og "[:upper:]" for å representere alle store bokstaver. Så bruker vi kommandoen "echo" for å skrive ut den kapitaliserte strengen til skjermen.

##Dykk dypere
Det er verdt å merke seg at denne metoden for å kapitalisere strenger i Bash ikke støtter mer enn et enkelt sett med tegn. For å kunne endre mer enn et sett, må du bruke et skript eller en løkke. Du kan også bruke andre kommandoer som "sed" eller "awk" for å oppnå det samme resultatet.

En annen ting å merke seg er at dette bare endrer utseendet på strengen, men ikke selve variabelen. Dette betyr at hvis du senere bruker variabelen, vil den fortsatt være skrevet i små bokstaver. For å permanent endre variabelen til en kapitalisert versjon, kan du bruke kommandoen "export" og deretter se på dokumentasjonen for "tr" for å utforske flere muligheter for substitusjon.

##Se også
1. [Bash-triks som vil gjøre livet ditt lettere](https://www.alias.tm/tutorials/2016/06/bash-tricks.html)
2. [The Bash Academy - en ressurs for å lære Bash-programmering](https://www.bash.academy/)