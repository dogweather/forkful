---
title:                "Bash: Stor bokstav i en streng"
simple_title:         "Stor bokstav i en streng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Har du noen gang lurt på hvordan du kan få en tekst til å se mer profesjonell ut? Eller kanskje du bare vil lære deg et nytt triks i Bash-programmering? Uansett grunn, er det nyttig å kunne formatere tekst, og en av måtene å gjøre det på er å kapitalisere en streng. I denne bloggposten vil vi se på hvorfor det kan være nyttig å kapitalisere en streng, og hvordan du gjør det i Bash.

## Hvordan
For å kapitalisere en streng i Bash er det noen få enkle trinn du kan følge. For det første må du definere strengen du vil kapitalisere som en variabel. La oss for eksempel si at strengen vår er "hei, dette er en test".

```Bash
streng="hei, dette er en test"
```

Deretter bruker du kommandoen `tr` for å gjøre om alle små bokstaver i strengen til store bokstaver.

```Bash
stor_streng=$(echo "$streng" | tr '[:lower:]' '[:upper:]')
```

Merk at vi lagrer den kapitaliserte strengen i en ny variabel kalt `stor_streng`, slik at vi beholder den originale strengen i `streng`-variabelen. Nå kan du skrive ut begge strengene for å se forskjellen.

```Bash
echo "Original streng: $streng"
echo "Kapitalisert streng: $stor_streng"
```

Det vil gi følgende utdata:

```
Original streng: hei, dette er en test
Kapitalisert streng: HEI, DETTE ER EN TEST
```

Det er også verdt å merke seg at den originale strengen forblir uendret, og bare den kapitaliserte versjonen er lagret i `stor_streng`-variabelen.

## Dypdykk
Å kapitalisere en streng er et enkelt og nyttig triks, men det er viktig å forstå hva som skjer under overflaten. Når vi bruker kommandoen `tr`, erstattes hver bokstav i strengen med en annen bokstav basert på definerte regler. I dette tilfellet bruker vi `[:lower:]` og `[:upper:]` for å definere hvilke bokstaver som skal erstattes.

Det finnes også andre måter å kapitalisere en streng på i Bash, som å bruke en løkke eller et regex-uttrykk. Men `tr`-kommandoen er ofte den enkleste og mest effektive måten å gjøre det på.

## Se Også
Hvis du er interessert i å lære mer om Bash-programmering, kan du sjekke ut følgende ressurser:

- [Bash-dokumentasjonen](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash Guide for nybegynnere](https://tldp.org/LDP/Bash-Beginners-Guide/html/index.html)
- [Bash-skripting for systemadministratorer](https://tldp.org/LDP/abs/html/)

Nå som du vet hvordan du kan kapitalisere en streng i Bash, håper vi at det vil være nyttig for deg i dine programmeringsprosjekter! Lykke til!