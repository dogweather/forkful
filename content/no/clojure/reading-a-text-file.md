---
title:                "Å lese en tekstfil"
html_title:           "Clojure: Å lese en tekstfil"
simple_title:         "Å lese en tekstfil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese en tekstfil er en grunnleggende ferdighet i mange programmeringsspråk, inkludert Clojure. Det lar deg hente informasjon fra en fil og bruke den i programmet ditt på en effektiv måte.

## Hvordan gjøre det

Lesing av en tekstfil involverer følgende trinn:

1. Åpne filen ved hjelp av `clojure.java.io/reader` funksjonen. Den tar inn filbanen som et argument.
2. Bruk `line-seq` funksjonen for å splitte filen inn i linjer og lagre dem i en variabel.
3. Gå gjennom hver linje ved hjelp av `doseq` eller `for` løkker.
4. Gjør noe med informasjonen fra hver linje, for eksempel skriv den ut eller lagre den i en liste.

La oss se på et eksempel der vi leser en tekstfil som inneholder navnene på frukt på hver linje og skriver dem ut:

```Clojure
(def file "fruits.txt")
(with-open [rdr (clojure.java.io/reader file)]
    (let [lines (line-seq rdr)]
        (doseq [line lines]
            (println line))))
```

Dette eksempelet illustrerer hvordan vi kan gi filbanen som et argument til `clojure.java.io/reader` funksjonen og bruke `line-seq` funksjonen til å splitte filen inn i linjer. Deretter bruker vi en `doseq` løkke for å skrive ut hver linje.

## Dykke dypere

Når du leser en tekstfil i Clojure, er det verdt å merke seg følgende:

- `with-open` funksjonen håndterer automatisk å lukke filen for deg når du er ferdig med å bruke den.
- `line-seq` funksjonen returnerer en sekvens av linjer i filen, slik at du kan bruke alminnelige Clojure funksjoner som `map` og `filter` på den.
- Det er også mulig å lese en tekstfil som en hel streng ved å bruke `slurp` funksjonen.

## Se også

- [clojure.java.io - Offisiell dokumentasjon](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Lær deg Clojure - Lesing av filer](https://learnxinyminutes.com/docs/no-no/clojure-no/)