---
title:    "Clojure: Sjekke om en mappe eksisterer"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Når vi programmerer i Clojure, er det viktig å være sikker på at vi sjekker eksistensen av en mappe før vi bruker den i programmet vårt. Dette sikrer at programmet vårt kjører feilfritt og unngår unødvendige feil.

## Hvordan

Det er viktig å bruke funksjonen ```clojure(fs/file? <dir>)``` for å sjekke om en mappe eksisterer eller ikke. Denne funksjonen returnerer en sann eller falsk verdi, avhengig av om mappen finnes eller ikke. Her er et eksempel på hvordan vi kan bruke denne funksjonen:

```Clojure
(if (fs/file? "min/mappe")
  (println "Mappen eksisterer!")
  (println "Mappen eksisterer ikke!"))
```

## Dypdykk

Når vi bruker funksjonen ```clojure(fs/file? <dir>)```, er det viktig å merke seg at den også vil returnere en sann verdi hvis det er en fil med samme navn som mappen vi sjekker for. Dette kan føre til unødvendige feil i koden vår. Derfor er det viktig å være spesifikk og sikre at vi kun sjekker for mapper.

En annen ting å huske på er at denne funksjonen ikke sjekker for eventuelle undermapper som kan eksistere inne i mappen vi sjekker. Dette kan være en potensiell kilde til feil, så det er viktig å inkludere sjekker for undermapper hvis det er nødvendig.

Det kan også være lurt å vurdere å bruke funksjonen ```clojure(fs/directory? <dir>)```, som spesifikt sjekker om en gitt filbane er en mappe eller ikke.

## Se også

* [Clojure FileSystem Dokumentasjon](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/file)
* [Clojure FileSystem Operations](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/file)
* [Checking for a file or directory in Clojure](https://brianhicks.github.io/2015/04/20/jvm-clojure-file-operations/)