---
title:    "Bash: Konvertere en dato til en streng"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en streng er et viktig konsept innenfor Bash programmering, spesielt når man jobber med filbehandling og systemadministrasjon. Ved å forstå hvordan man konverterer en dato til en streng, kan man lage skript som håndterer filnavn, loggfiler og andre systemoppgaver som er avhengige av datoer.

## Slik gjør du det

For å konvertere en dato til en streng i Bash, bruker vi kommandoen `date` sammen med formatstrings. Formatstrings lar oss spesifisere hvordan vi ønsker at datoen skal vises som en streng. La oss si at vi vil konvertere dagens dato til en streng og lagre den i en variabel kalt `date_string`. Dette er hvordan koden vår kunne sett ut:

````Bash
date_string=$(date +"%d-%m-%Y")
echo $date_string
````

I dette tilfellet bruker vi formatstringen `%d-%m-%Y` som betyr at datoen skal vises i formatet "dag-måned-år". Kommandoen `$( )` lar oss lagre outputen fra `date` kommandoen i variabelen `date_string`. Når vi så kaller på variabelen med `echo` kommandoen, vil datoen bli skrevet ut i ønsket format. 

La oss si at vi vil inkludere dagens dato i et filnavn. Vi kan da bruke `date_string` variabelen og inkludere den i filnavnet som vi kan lagre i en annen variabel som `file_name`.

````Bash
file_name="log_$(date +"%d-%m-%Y").log"
echo $file_name
````

I dette tilfellet vil `file_name` variabelen inneholde "log_dag-måned-år.log". Dette viser hvordan vi kan bruke konverteringen av dato til streng i praktiske situasjoner.

## Dypdykk

Det finnes mange forskjellige formatstrings som kan brukes i sammenheng med `date` kommandoen. Her er noen eksempler på hvordan forskjellige formatstrings vil se ut:
- `%d` viser dagens dato som et nummer fra 01-31.
- `%m` viser måneden som et nummer fra 01-12.
- `%Y` viser året som et 4sifret tall (f.eks. 2021).
- `%H` viser timen i 24-timers format (f.eks. 13 for klokken 1 på ettermiddagen).
- `%M` viser minuttene som et tall fra 00-59.
- `%S` viser sekundene som et tall fra 00-59.

Fullstendig liste over formatstrings og deres betydning kan finnes i `date` kommandoen sin manual ved å kjøre `man date` i terminalen. 

Det er også verdt å nevne at vi kan kombinere forskjellige formatstrings for å få ønsket resultat. For eksempel kan vi bruke `%d-%m-%Y-%H-%M-%S` for å få både dato og klokkeslett i filnavnet vårt.

## Se også

- [Bash manual](https://www.gnu.org/software/bash/manual/)
- [Guide til Bash variabler](https://www.vikingcodeschool.com/web-development-basics/a-better-understanding-of-bash)
- [Bash scripting tutorials](https://www.tldp.org/LDP/abs/html/)