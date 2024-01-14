---
title:                "Clojure: Sjekke om en mappe finnes"
simple_title:         "Sjekke om en mappe finnes"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor 

Å sjekke om en mappe eksisterer kan være en viktig del av programmeringen for å sikre at koden fungerer som den skal. Det kan spare deg for problemer og feil når du prøver å åpne eller lagre filer.

## Hvordan gjøre det

Å sjekke om en mappe eksisterer i Clojure er enkelt. Du kan bruke Clojure-funksjonen "file-exists?" for å sjekke om en mappe finnes på en gitt sti. Her er et eksempel på hvordan du kan gjøre det:

```Clojure
(if (file-exists? "sti/til/mappe")
  (println "Mappen eksisterer!")
  (println "Mappen eksisterer ikke."))
```

Outputen av dette vil være enten "Mappen eksisterer!" eller "Mappen eksisterer ikke." avhengig av om mappen finnes eller ikke.

## Dypdykk

Det er også mulig å sjekke om en mappe eksisterer ved hjelp av Clojure-funksjonen "dir?" som returnerer sann hvis det er en gyldig mappe og usann hvis det ikke er det. Du kan også bruke "direktori?" -funksjonen for å gjøre det samme. Disse funksjonene gir litt mer detaljert informasjon om mappen, som for eksempel størrelse og endringsdato.

En annen viktig ting å merke seg er at hvis du bruker en relativ sti i "file-exists?" -funksjonen, vil den sjekke om mappen eksisterer relativt til din nåværende arbeidsmappe. Men hvis du bruker en absolutt sti, vil den sjekke om mappen eksisterer uavhengig av din nåværende arbeidsmappe.

## Se også

- [Clojure dokumentasjon for file-exists?](https://clojure.org/api/clojure.java.io/file_exists_qmark)
- [Mer om filbehandling i Clojure](https://oreilly.com/library/view/clojure-cookbook/9781449366406/ch04s17.html)
- [En nybegynnerveiledning for Clojure programmering](https://clojure.org/guides/getting_started)