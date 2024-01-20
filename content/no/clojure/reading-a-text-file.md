---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å lese en tekstfil innebærer å hente data eller informasjon lagret i en fil. Programmerere gjør dette for å manipulere, tolke og benytte denne dataen i ulike applikasjoner.

## Hvordan gjøre det:

I Clojure kan du bruke `slurp` funksjonen for å lese en tekstfil. Denne eksempelkoden vil lese filen "test.txt":

```clojure
(defn read-file [filename]
  (slurp filename))

(println (read-file "test.txt"))
```

Hvis "test.txt" inneholder informasjon som "Hei, Verden!", vil output være:

```clojure
"Hei, Verden!"
```

## Dypdykk

1. Historisk kontekst: I tidlige datamaskintider, filer ble lest og skrevet i binær form. Lesing av tekstfiler i høy-nivå språk som Clojure, gjør manipulering og tolkning av data enklere og mer intuitiv.

2. Alternativer: Det er andre funksjoner for å lese filer i Clojure som `line-seq` som returnerer en sekvens av linjer i filen, eller `read-line` som bare leser den neste linjen.

3. Implementasjonsdetaljer: `Slurp` funksjonen leser hele filen i hukommelsen på en gang, noe som kan være et problem for store filer. Alternativt, kan du bruke en `lazy-seq` funksjon som `line-seq` for å lese store filer effektivt.

## Se også:

1. Clojure Dokumentasjon på `slurp`: https://clojuredocs.org/clojure.core/slurp
2. Clojure Dokumentasjon på `line-seq`: https://clojuredocs.org/clojure.core/line-seq
3. Tutorial om å lese og skrive filer i Clojure: https://www.tutorialspoint.com/clojure/clojure_file_io.htm
4. Spørsmål og svar om å lese filer i Clojure: https://stackoverflow.com/questions/37412482/clojure-read-file-line-by-line