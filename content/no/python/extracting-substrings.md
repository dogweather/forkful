---
title:                "Python: Utdraging av delstrenger"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/extracting-substrings.md"
---

{{< edit_this_page >}}

##Hvorfor

Noen ganger i programmering kan vi ha behov for å hente ut en spesifikk del av en tekststreng. Dette kalles "substring extraction" på engelsk, og kan være nyttig i mange ulike situasjoner. I denne bloggposten skal vi se nærmere på hvorfor og hvordan man kan gjøre dette ved hjelp av Python.

##Hvordan gjøre det

For å ekstrahere deler av en tekststreng i Python, kan vi bruke en metode kalt "slice". Dette gjøres ved å indikere hvilke indekser vi ønsker å hente ut fra strengen, og deretter skrive disse indeksene mellom hakete klammer. La oss se et eksempel:

```Python
tekst = "Denne teksten er til eksperimentell bruk"

print(tekst[6:12])
```

Dette eksempelet vil gi oss output-en "teksten". Her har vi brukt slice-metoden til å hente ut de indeksene som tilsvarer ordet "teksten" i tekststrengen vår.

Vi kan også bruke slice-metoden til å hente ut deler av en tekststreng ved hjelp av negative indekser. Dette betyr at vi teller baklengs fra slutten av strengen. La oss se et annet eksempel:

```Python
tekst = "Dette er en veldig lang setning"

print(tekst[-9:-1])
```

Output-en her vil være "setning". Vi brukte de negative indeksene -9 og -1 for å hente ut ordet "setning".

##Dypdykk

For å forstå hvorfor slice-metoden fungerer, må vi se nærmere på hvordan Python håndterer tekststrenger. En tekststreng består av en rekke enkeltelementer, også kalt "characters". Disse elementene har hver sin plass i strengen, også kalt "indekser". Når vi bruker slice-metoden, forteller vi Python hvilke indekser vi ønsker å utheve og hente ut.

Det er også verdt å nevne at vi kan bruke slice-metoden til å traverse gjennom en tekststreng og ekstrahere hvert n-te element ved hjelp av et tredje argument. La oss se et eksempel på dette:

```Python
tekst = "ABCDEFG"

print(tekst[::2])
```

Her vil output-en være "ACEG". Vi brukte her det tredje argumentet (i dette tilfellet 2) til å hoppe over hver andre bokstav i strengen og hente ut kun disse. Dette kan være nyttig hvis vi ønsker å hente ut hvert annet element, eller hvert tredje element osv.

##Se også
- [Python tutorial](https://docs.python.org/3/tutorial/index.html)
- [String methods](https://www.w3schools.com/python/python_ref_string.asp)

Vi håper dette var nyttig for deg for å forstå hvordan du kan hente ut deler av en tekststreng ved hjelp av slice-metoden i Python. God programmering!