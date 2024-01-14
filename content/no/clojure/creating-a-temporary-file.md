---
title:    "Clojure: Lage en midlertidig fil"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange grunner til å programmere i Clojure, og en av dem kan være å skape midlertidige filer. Dette kan være nyttig for å lagre data eller utføre midlertidige oppgaver mens man arbeider med et større prosjekt.

## Slik gjør du det

Å opprette en midlertidig fil i Clojure er enkelt og kan gjøres ved hjelp av funksjonen `with-open` og `java.io/FileWriter`. Først må vi importere `java.io.File` og `java.io.FileWriter` i vårt Clojure-prosjekt:

```Clojure
(ns mitt.prosjekt
  (:import [java.io File FileWriter]))
```

Deretter kan vi bruke `with-open` for å opprette vår midlertidige fil, og `java.io.FileWriter` for å skrive til filen:

```Clojure
(with-open [fil (File/createTempFile "mittprosjekt-" ".txt")]
  (let [skriver (FileWriter. fil)]
    (.write skriver "Dette er innholdet i vår midlertidige fil.")
    (.close skriver)))
```

Her oppretter vi en midlertidig fil med prefix "mittprosjekt-" og filextension ".txt". Deretter bruker vi `FileWriter` til å skrive til filen ved hjelp av `.write`-metoden, og til slutt lukker vi filen ved hjelp av `.close`-metoden.

## Dypdykk

Når man oppretter en midlertidig fil i Clojure, blir filen automatisk slettet når `with-open`-blokken avsluttes. Dette er en sikkerhetsfunksjon som sørger for at filen ikke blir liggende igjen i systemet.

Man kan også angi en filsti som første parameter i `File/createTempFile`. Dette gjør det mulig å opprette midlertidige filer i spesifikke mapper.

```Clojure
(with-open [fil (File/createTempFile "/bruker/mittprosjekt/" "temp-" ".txt")]
  (let [skriver (FileWriter. fil)]
    (.write skriver "Dette er innholdet i en midlertidig fil opprettet i /bruker/mittprosjekt-mappen.")
    (.close skriver)))
```

Det er også mulig å angi en låsende flagg ved hjelp av `:lockable true` som tredje parameter i `File/createTempFile`, dersom man ønsker å sikre at andre prosesser ikke kan lese fra filen mens den er åpen.

## Se også

Her er noen nyttige ressurser for å lære mer om å opprette midlertidige filer i Clojure:

- [ClojureDocs - createTempFile](https://clojuredocs.org/java.io.File/createTempFile)
- [Official Clojure website](https://clojure.org/)
- [Clojure for the Brave and True](https://www.braveclojure.com/) - en nybegynnerguide til Clojure