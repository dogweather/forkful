---
title:                "Å lese en tekstfil."
html_title:           "Clojure: Å lese en tekstfil."
simple_title:         "Å lese en tekstfil."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Lesing av en tekstfil er å åpne og lese innholdet av en fil som inneholder tekst. Programmere gjør dette for å kunne manipulere og bruke informasjonen som finnes i filen, for eksempel for å bearbeide data eller hente ut spesifikke deler av teksten.

## Slik gjør du det:
Bruk funksjonen `read-string` i Clojure for å åpne og lese en tekstfil. Her er et eksempel på hvordan du kan lese innholdet i en fil kalt "tekstfil.txt":

```Clojure
(def tekst (slurp "tekstfil.txt"))
```

Dette vil lagre innholdet i filen i variabelen `tekst`. Du kan deretter manipulere og bruke informasjonen som du ønsker.

## Dypdykk:
Å lese tekstfiler har vært en grunnleggende funksjon i programmering siden starten. Før kunne datamaskiner kun behandle tekstfiler, og de ble brukt til å lagre og dele informasjon. Alternativet til å lese tekstfiler i dag er å bruke en database eller et API. Når du åpner og leser en tekstfil i Clojure, blir innholdet lagret som en streng og kan manipuleres med funksjoner som `substring` og `replace`.

## Se også:
[Lesing av filer i Clojure dokumentasjon](https://clojure-doc.org/index.html#read-file)