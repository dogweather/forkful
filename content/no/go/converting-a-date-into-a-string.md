---
title:                "Konvertering av en dato til en streng"
html_title:           "Go: Konvertering av en dato til en streng"
simple_title:         "Konvertering av en dato til en streng"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##Hvorfor
Å konvertere en dato til en streng kan være nyttig når du for eksempel ønsker å vise datoen på en leserlig måte eller lagre den i en database. Det kan også være nødvendig når du håndterer brukerinput eller sender datoen som en del av en API-forespørsel.

##Hvordan gjøre det
Å konvertere en dato til en streng i Go er enkelt og krever bare noen få linjer med kode. Først må du importere "time" pakken:

```Go
import "time"
```

Deretter kan du bruke "Format" funksjonen for å konvertere datoen til en streng:

```Go
time.Now().Format("January 02, 2006")
```

Dette vil gi følgende output:

```Go
August 27, 2021
```

Du kan også spesifisere et annet format for datoen ved å endre formatstrengen. For eksempel:

```Go
time.Now().Format("02/01/2006")
```

Dette vil gi følgende output:

```Go
27/08/2021
```

##Dypdykk
Når du konverterer en dato til en streng, kan du også inkludere andre verdier som timer, minutter og sekunder i formatstrengen. Dette kan være nyttig hvis du trenger å vise en mer detaljert datoen til brukeren.

Du kan også bruke funksjoner som "Add" og "Sub" for å legge til eller trekke fra et visst antall timer, minutter eller sekunder fra en dato før du konverterer den til en streng.

Det er også verdt å merke seg at formatstrenger i Go følger et spesielt mønster, hvor spesifikke bokstaver representerer forskjellige verdier av datoen. En full oversikt over disse bokstavene finner du i dokumentasjonen til "time" pakken.

##Se også
- "time" pakken dokumentasjon: https://golang.org/pkg/time/
- Konvertering av strenger til dato i Go: https://www.digitalocean.com/community/tutorials/how-to-format-date-and-time-in-the-go-programming-language