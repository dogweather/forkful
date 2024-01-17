---
title:                "Lage en midlertidig fil"
html_title:           "Clojure: Lage en midlertidig fil"
simple_title:         "Lage en midlertidig fil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å opprette en midlertidig fil er en vanlig oppgave for programmerere. Dette innebærer å opprette en fil som bare skal være tilgjengelig for en kort periode, vanligvis i løpet av programkjøringen. Dette gjøres ofte for å lagre midlertidige data eller for å håndtere filoperasjoner på en sikrere måte.

## Hvordan:

```Clojure
(require '[clojure.java.io :as io])

;; Opprett en midlertidig fil med et unikt navn
(def temp-file (io/file "temp_file.txt"))

;; Skriv til filen
(with-open [w (io/writer temp-file)]
  (.write w "Dette er en midlertidig fil."))

;; Les fra filen
(io/slurp temp-file)

;; Slett filen når den ikke lenger er nødvendig
(io/delete-file temp-file)
```

Output: ```"Dette er en midlertidig fil."```

## Dypdykk:

Å opprette midlertidige filer er ikke noe nytt, det er en vanlig praksis som har blitt brukt i mange år. Alternativet til å opprette en midlertidig fil er å bruke minnebufferen istedenfor, men dette kan føre til at data går tapt ved uventede avbrudd. Midlertidige filer gir derfor en mer pålitelig måte å håndtere data på.

I Clojure er det flere måter å opprette midlertidige filer på, både ved hjelp av standardbiblioteket og tredjepartsbiblioteker som Raynes' Tempura. Det anbefales å bruke standardbiblioteket hvis man har det tilgjengelig, da man da unngår å legge til unødvendige avhengigheter i prosjektet.

Implementeringen av opprettelse av midlertidige filer i Clojure er basert på Java's [File.createTempFile()](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-java.io.File-) metode. Denne metoden oppretter en fil med et unikt navn og legger den i operativsystemets midlertidige mappe.

## Se Også:

- [Clojure's Java IO namespace](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Tempura by Raynes](https://github.com/Raynes/tempura)
- [Java's File.createTempFile() metode](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-java.io.File-)