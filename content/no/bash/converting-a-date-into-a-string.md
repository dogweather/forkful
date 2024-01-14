---
title:    "Bash: Konvertere en dato til en streng"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Å konvertere en dato til en streng er en vanlig oppgave i Bash programmering. Dette gjøres for å kunne presentere datoen på en mer lesbar og forståelig måte. Det kan også være nyttig for å formatere datoer i forbindelse med lagring eller bruk i andre programmer.

## Slik gjør du det
Å konvertere en dato til en streng i Bash er en enkel prosess som kan gjøres ved hjelp av innebygde funksjoner og variabler. Først må vi definere datoen vi ønsker å konvertere ved hjelp av kommandoen `date`. Deretter kan vi bruke formatet `%Y-%m-%d` for å få datoen som et tall. Til slutt kan vi bruke variabelen `BASHPID` for å konvertere datoen til en streng.

```Bash
# Definer datoen
date="2021-01-01"

# Konverter datoen til en streng
date_string=$(date -d "$date" +"%Y-%m-%d")

# Skriv ut resultatet
echo "Datoen som streng: $date_string"

# Output: Datoen som streng: 2021-01-01
```

Datoen kan også konverteres til andre formater, som for eksempel måned og år eller dag og måned, ved å endre formatet i kommandoen `date`. Du kan lese mer om de ulike formateringsmulighetene ved å kjøre `man date` i Bash-terminalen.

## En dypere forståelse
For å forstå hvordan konverteringen fra en dato til en streng fungerer, er det nyttig å vite at datoen i Bash representeres som et tall på formatet `YYYYMMDD`, hvor Y står for år, M står for måned og D står for dag. Ved å bruke formatet `%Y-%m-%d` i `date`-kommandoen, omformes tallet til å bli på formatet `YYYY-MM-DD`, som er mer leselig for mennesker.

Det er også verdt å merke seg at `date`-kommandoen kan ha ulik funksjonalitet på ulike operativsystemer, så det kan være lurt å lese dokumentasjonen for din spesifikke versjon av Bash for mer detaljert informasjon.

## Se også
- [How to Use Bash Date and Time Commands](https://www.cyberciti.biz/faq/unix-linux-bash-get-time/)
- [The `date` Command in Bash](https://www.baeldung.com/linux/use-date-command-bash)
- [Bash Date Conversion and Formatting](https://tecadmin.net/working-with-date-in-bash-scripting/)