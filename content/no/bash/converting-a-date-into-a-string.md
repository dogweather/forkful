---
title:                "Konvertere en dato til en streng"
html_title:           "Bash: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?

Konvertering av en dato til en streng er en vanlig oppgave i programmering som innebærer å gjøre om en dato i et bestemt format til en tekststreng som kan leses og tolkes av datamaskiner. Dette gjøres ofte for å lagre, behandle eller presentere datoer i et mer leselig format.

Hvordan:
Her er to enkle eksempler på hvordan du kan konvertere en dato til en streng i Bash-programmering:

```Bash
# Eksempel 1:
date=`date +"%d-%m-%Y"`  # Lagrer dagens dato i variabelen "date"
echo "I dag er den $date" # Output: I dag er den 24-11-2021

# Eksempel 2:
myDate="2021-11-24" # Oppretter en variabel med dato i ISO-format
newDate=$(date -d "$myDate" +"%A, %d %B %Y") # Konverterer datoen til ønsket format
echo "Dagen i dag er $newDate" # Output: Dagen i dag er Wednesday, 24 November 2021
```

Deep Dive:
Konvertering av dato til streng har eksistert i mange år og har vært en del av programmeringsspråk som C og Java siden starten. Det finnes også alternative måter å gjøre dette på, som å bruke innebygde funksjoner i andre programmeringsspråk som Python eller PHP.

Implementasjonen av konvertering av dato til streng i Bash bruker kommandoen "date" og inneholder mange ulike formateringsmuligheter som kan passe til ulike formål. Det kan også være lurt å ta hensyn til lokale innstillinger og språk når man konverterer datoer til strenger.

Se også:
- [Date and Time Conversion in Bash](https://linuxize.com/post/bash-date-command/)
- [How to Convert Dates to Different Formats in Bash](https://ostechnix.com/how-to-convert-dates-to-different-formats-in-bash/)
- [ISO 8601 Standard](https://www.iso.org/iso-8601-date-and-time-format.html)