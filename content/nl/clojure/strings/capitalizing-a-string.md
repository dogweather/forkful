---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:19.373119-07:00
description: "Een string kapitaliseren betekent dat je de eerste letter hoofdletter\
  \ maakt en de rest kleine letters. We doen dit om gegevens te normaliseren en de\u2026"
lastmod: '2024-03-13T22:44:50.401453-06:00'
model: gpt-4-0125-preview
summary: Een string kapitaliseren betekent dat je de eerste letter hoofdletter maakt
  en de rest kleine letters.
title: Een string met hoofdletters maken
weight: 2
---

## Hoe te:
In Clojure is er geen ingebouwde functie om strings direct te kapitaliseren. Je maakt er zelf een met de `clojure.string` bibliotheek. Hier is een snelle manier:

```clojure
(require '[clojure.string :as str])

(defn capitalize [s]
  (when s
    (str/capitalize s)))

(capitalize "hello world") ; => "Hello world"
```

Voorbeelduitvoer voor de `capitalize` functie:

```clojure
(capitalize "clojure") ; => "Clojure"
(capitalize "123clojure") ; => "123clojure"
(capitalize "") ; => nil
(capitalize nil) ; => nil
```

## Diepere Duik
Clojure's standaardbibliotheek, `clojure.string`, prefereert eenvoud. Daarom geen kant-en-klare `capitalize` functie zoals je die in andere talen zou vinden. Historisch gezien leunt Clojure op Java's String methoden, die basismanipulatie bieden, maar geen `capitalize`.

Dit gebrek drijft je ertoe om ofwel je eigen oplossing te schrijven, zoals hierboven, of externe bibliotheken te gebruiken. Er is ook een `capitalize` van `clojure.contrib.string`, een historisch aparte contrib bibliotheek voordat deze werd afgeschaft en deels samengevoegd met clojure.string in latere versies.

De eenvoud van de `str/capitalize` functie betekent dat het alleen bezig is met het eerste teken. Voor meer genuanceerde kapitalisatie, zoals titelcasus of het omgaan met internationale karakters, moet je een aangepaste oplossing schrijven of een Java-bibliotheek gebruiken.

Hier is een alternatieve aangepaste functie die omgaat met strings met meerdere woorden:

```clojure
(defn title-case [s]
  (->> s
       (str/split #"\s+")
       (map str/capitalize)
       (str/join " ")))

(title-case "the lord of the rings") ; => "The Lord Of The Rings"
```

Opnieuw wordt internationalisering (i18n) hier niet behandeld; het correct omgaan met Unicode is een heel ander beest, vaak vereist het gespecialiseerde bibliotheken.

## Zie Ook
- Clojure Strings API: https://clojure.github.io/clojure/clojure.string-api.html
- Java String Documentatie: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html
- Clojure Contrib bibliotheek (gearchiveerd): https://github.com/clojure/clojure-contrib
- `clojure.string` broncode: https://github.com/clojure/clojure/blob/master/src/clj/clojure/string.clj
