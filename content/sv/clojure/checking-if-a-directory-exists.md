---
title:                "Kontrollera om en katalog finns"
html_title:           "Arduino: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att kontrollera om en katalog finns innebär att du verifierar att en specifik mapp existerar i filsystemet. Programmerare gör detta för att undvika fel vid filoperationer, såsom läsning från eller skrivning till en mapp som inte finns.

## Så här gör du:

Här är hur du kan kolla om en mapp finns med hjälp av Clojure:

```clojure
(require '[clojure.java.io :as io])

(defn directory-exists? [dir]
  (.exists (io/file dir)))

(println (directory-exists? "/path/to/your/directory")) ; Byt ut mot din sökväg
(println (directory-exists? "/path/that/does/not/exist"))
```

Sample output:

```clojure
true
false
```

## Fördjupning:

Historiskt sett har filsystemshanteringen varierat beroende på operativsystem, men JVM (Java Virtual Machine) har standardiserat dessa operationer för Clojure. Alternativen till `clojure.java.io` kunde inkludera anrop till shell-kommandon eller användning av lågnivå Java-filsystem-API:er. Klojurs enkelhet i `io/file` och relaterade funktioner döljer de underliggande komplexa Java-implementationsdetaljerna.

## Se även:

Här är några länkar för vidare läsning och relaterade resurser:

- Clojure's `clojure.java.io` doc: [https://clojuredocs.org/clojure.java.io](https://clojuredocs.org/clojure.java.io)
- Mer om JVM och filsystem: [https://docs.oracle.com/javase/tutorial/essential/io/](https://docs.oracle.com/javase/tutorial/essential/io/)
- Officiell Clojure-guide till Java Interoperability: [https://clojure.org/reference/java_interop](https://clojure.org/reference/java_interop)
