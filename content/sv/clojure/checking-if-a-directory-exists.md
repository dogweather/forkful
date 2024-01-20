---
title:                "Kontrollera om en katalog finns"
html_title:           "Bash: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog finns innebär att vi avgör om en specifik mappläge existerar i filsystemet. Programmerare gör detta för att undvika fel som uppstår när de försöker läsa, skriva eller manipulera icke-existerande kataloger.

## Hur man gör:
I Clojure, vi kan använda `java.nio.file` för att kontrollera om en katalog finns.

Först, importera nödvändiga klasser:
```Clojure
(import '[java.nio.file Files Paths])
```

Sedan, skapa en Path-objekt av katalogen:
```Clojure
(def my-path (Paths/get "/example/directory" (into-array String [])))
```

Till sist, använd `Files/exists` för att kontrollera om katalogen finns:
```Clojure
(defn directory-exists? [path]
  (Files/exists path))
```

Prova denna funktion:
```Clojure
(directory-exists? my-path)
```

Om katalogen existerar, resultatet kommer att vara `true`. Annars, kommer det att vara `false`.

## Djupdykning
Historiskt sett, kontroll av om en katalog finns var inte alltid ett inbyggt koncept i tidiga programmeringsspråk. I Unix baserade system, till exempel, utvecklare var tvungna att försöka skapa eller öppna den angivna katalogen och fånga ett undantag om det misslyckades.

Alternativt, i Clojure kan du använda `clojure.java.io/file` och ` .exists` metoden:
```Clojure
(import '[clojure.java.io file])

(defn directory-exists? [dir]
  (.exists (file dir)))
```
Men, `java.nio.file` paketet ger en mer robust lösning för att hantera filsystemet.

## Se också
Se "Java Platform, Standard Edition File I/O Tutorial" för mer detaljer om `java.nio.file`: https://docs.oracle.com/javase/tutorial/essential/io/fileio.html

För mer om Clojure och Java interoperation, se den officiella Clojure-dokumentationen: https://clojure.org/reference/java_interop