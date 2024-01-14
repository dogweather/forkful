---
title:    "Clojure: Lese en tekstfil"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese en tekstfil er en vanlig oppgave for programmerere, og som en Clojure-utvikler vil du sannsynligvis også komme over denne oppgaven. Enten det er å hente data fra en fil eller å analysere og behandle informasjon, er det viktig å vite hvordan du kan lese en tekstfil effektivt. I denne bloggposten skal vi se på hvordan du kan gjøre dette i Clojure.

## Hvordan

Det første vi må gjøre er å definere en filbane til tekstdokumentet vårt. Vi kan gjøre dette ved hjelp av funksjonen `file`, som tar inn en streng som representere filbanen vår. La oss si at vi har en tekstfil som heter "tekstfil.txt" som ligger i samme mappe som vår Clojure-fil, da vil filbanen vår se slik ut:

```Clojure
(def file-path (file "tekstfil.txt"))
```

Nå som vi har definert filbanen vår, kan vi bruke funksjonen `slurp` for å lese innholdet av filen. Denne funksjonen tar inn filbanen som et argument og returnerer en streng med alt innholdet i filen. La oss se på et eksempel:

```Clojure
(def file-content (slurp file-path))
(println file-content)
```

Dette vil skrive ut alt innholdet i tekstfilen til konsollen. Hvis filen vår inneholder følgende tekst:

```
Dette er en tekstfil.
Her er litt data å lese.
```

Så vil outputen bli:

```
Dette er en tekstfil.
Her er litt data å lese.
```

## Deep Dive

Å lese en tekstfil er en relativt enkel oppgave i Clojure, men det er viktig å være oppmerksom på at `slurp` funksjonen leser hele filen inn i minnet. Hvis tekstfilen vår er veldig stor, kan dette føre til problemer med ytelse og ressursbruk. I så fall kan det være mer effektivt å bruke funksjonen `line-seq`, som vil lese filen linje for linje og returnere en sekvens av linjer.

Vi kan også spesifisere en annen encoding for å lese tekstfilen, ved hjelp av `slurp` funksjonen. For eksempel, hvis filen vår er i utf-8 encoding, kan vi spesifisere dette som følger:

```Clojure
(slurp file-path :encoding "utf-8")
```

## Se Også

- Clojure dokumentasjon for `slurp`: https://clojuredocs.org/clojure.core/slurp
- Clojure dokumentasjon for `line-seq`: https://clojuredocs.org/clojure.core/line-seq
- Chris Schenkels Clojure for the Brave and True tutorial om å lese filer: http://www.braveclojure.com/reading-files/