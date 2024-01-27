---
title:                "Skriving av en tekstfil"
date:                  2024-01-19
html_title:           "Arduino: Skriving av en tekstfil"
simple_title:         "Skriving av en tekstfil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skrive tekstfiler er lagring av tekstdata til en fil. Programmerere gjør dette for å beholde data mellom kjøringer, dele info eller for logging.

## Hvordan:
```Clojure
; Bruk 'spit' for å skrive til en fil
(spit "eksempel.txt" "Hei, Clojure verden!")

; For å legge til tekst i eksisterende fil, sett append-flagget til true
(spit "eksempel.txt" "\nDette er en ny linje." :append true)
```
Output blir en fil "eksempel.txt" med innhold:
```
Hei, Clojure verden!
Dette er en ny linje.
```

## Dypdykk
I Clojure, er 'spit' en høy-nivå funksjon for å skrive data til filer, og kom som en del av Clojure I/O biblioteket for å gjøre filhåndtering enklere. Alternativer inkluderer lav-nivå Java I/O operasjoner via interop. 'Spit' er enkel, men for komplekse behov, bruk PrintWriter, BufferedWriter eller java.nio.file.Files klassen.

## Se Også:
- ClojureDocs for `spit`: https://clojuredocs.org/clojure.core/spit
- Clojure I/O guide: https://clojure.org/guides/io
- Clojure Java Interop: https://clojure.org/reference/java_interop
