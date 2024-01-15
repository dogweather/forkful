---
title:                "Å lage en midlertidig fil"
html_title:           "Clojure: Å lage en midlertidig fil"
simple_title:         "Å lage en midlertidig fil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Det kan være nyttig å opprette midlertidige filer i Clojure når du trenger å lagre midlertidige data eller når du jobber med operativsystemspesifikke funksjoner som krever en midlertidig fil. Ved å opprette en midlertidig fil, kan du enkelt lagre og utføre operasjoner på data uten å bekymre deg for å lagre det permanent eller manuelt slette filen etterpå.

## Slik gjør du det
For å opprette en midlertidig fil i Clojure, kan du bruke funksjonene `java.io.File/createTempFile` eller `java.nio.file.Files/createTempFile`. Begge funksjonene tar to argumenter - et prefiksnavn og en suffiksnavn - som brukes til å generere et unikt navn for den midlertidige filen.

Enkelt eksempel:

```Clojure
(require '[clojure.java.io :as io])

(io/file (io/temporary-directory) "temp" nil ".txt") 
```

Dette vil opprette en midlertidig fil med navnet "tempXXX.txt" (der XXX er et unikt tall) i den midlertidige mappen på ditt operativsystem.

## Dypere dykk
Når du oppretter en midlertidig fil, vil filen automatisk bli slettet når programmet ditt avsluttes. Men hvis du vil slette filen manuelt, kan du bruke funksjonen `java.io.File/deleteOnExit` for å angi at filen skal slettes når JVM-en avsluttes.

Du kan også angi midlertidige mappen der filen skal opprettes ved å bruke funksjonen `io/temporary-directory`. Standard plasseringen kan variere avhengig av operativsystemet, så det er nyttig å bruke denne funksjonen for å sikre en konsistent plassering.

## Se også
- [Clojure docs for java.io.File](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/file)
- [Clojure docs for java.nio.file.Files](https://clojure.org/reference/java_interop#_nio_paths)