---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:11.491733-07:00
description: "Assoziative Arrays, oder Hash-Maps, in Clojure erm\xF6glichen es Ihnen,\
  \ Daten mit Schl\xFCssel-Wert-Paaren zu speichern und abzurufen. Sie sind eine erste\
  \ Wahl\u2026"
lastmod: 2024-02-19 22:05:12.462094
model: gpt-4-0125-preview
summary: "Assoziative Arrays, oder Hash-Maps, in Clojure erm\xF6glichen es Ihnen,\
  \ Daten mit Schl\xFCssel-Wert-Paaren zu speichern und abzurufen. Sie sind eine erste\
  \ Wahl\u2026"
title: Verwendung von assoziativen Arrays
---

{{< edit_this_page >}}

## Was & Warum?

Assoziative Arrays, oder Hash-Maps, in Clojure ermöglichen es Ihnen, Daten mit Schlüssel-Wert-Paaren zu speichern und abzurufen. Sie sind eine erste Wahl für die Verwaltung strukturierter Daten, da sie einen schnelleren Zugriff auf bestimmte Elemente ermöglichen, ohne durch eine Liste iterieren zu müssen.

## Wie:

In Clojure ist das Erstellen und Manipulieren von assoziativen Arrays (Hash-Maps) unkompliziert. Lassen Sie uns mit Beispielen eintauchen.

Um eine Hash-Map zu erstellen:

```clojure
(def my-map {:name "Alex" :age 30})
```

Sie können einen Wert abrufen, indem Sie seinen Schlüssel angeben:

```clojure
(get my-map :name)
;; "Alex"
```
Oder, idiomatischer, können Sie den Schlüssel als Funktion verwenden:

```clojure
(:name my-map)
;; "Alex"
```

Das Hinzufügen oder Aktualisieren von Einträgen ist einfach:

```clojure
(def updated-map (assoc my-map :location "New York"))
;; {:name "Alex", :age 30, :location "New York"}

(def incremented-age (update my-map :age inc))
;; {:name "Alex", :age 31}
```

Um Schlüssel zu entfernen, verwenden Sie `dissoc`:

```clojure
(def removed-age (dissoc my-map :age))
;; {:name "Alex"}
```

Um über eine Map zu iterieren:

```clojure
(doseq [[k v] my-map] (println k "->" v))
;; :name -> Alex
;; :age -> 30
```

Und für bedingten Zugriff gibt `find` ein Schlüssel-Wert-Paar zurück, wenn der Schlüssel existiert:

```clojure
(find my-map :age)
;; [:age 30]
```

## Tiefer Eintauchen

Assoziative Arrays in Clojure, auch häufig als Hash-Maps bezeichnet, sind unglaublich vielseitig und effizient für die Verwaltung von datenbasierten Schlüssel-Wert-Daten. Sie sind Teil der umfangreichen Sammlungsbibliothek von Clojure, tief verwurzelt in der Philosophie der Unveränderlichkeit und der funktionalen Programmierung des Sprache. Im Gegensatz zu Arrays oder Listen, die eine O(n) Zeitkomplexität für den Zugriff auf Elemente benötigen, bieten Hash-Maps eine nahezu konstante Zeitkomplexität für den Zugriff, was sie für Suchvorgänge hochgradig effizient macht.

Man könnte argumentieren, dass Vektoren in Clojure einen ähnlichen Zweck durch indizierten Zugriff erfüllen könnten, aber Hash-Maps glänzen, wenn es um den Umgang mit nicht-sequentiellen und beschrifteten Daten geht, bei denen der Schlüssel einen sinnvollen Deskriptor anstelle eines willkürlichen Index bietet.

Einzigartig für Clojure (und sein Lisp-Erbe) sind assoziative Arrays Bürger erster Klasse, was bedeutet, dass sie direkt manipuliert, zwischen Funktionen übergeben und mehr werden können, ohne dass spezielle Syntax oder Zugriffsmethoden benötigt werden. Diese Designentscheidung verstärkt Clojures Betonung auf Einfachheit und Leistung.

Obwohl Hash-Maps unglaublich nützlich sind, ist es erwähnenswert, dass für sehr große Datensätze oder Szenarien, in denen Schlüssel hochdynamisch sind (ständige Addition und Entfernung), alternative Datenstrukturen oder Datenbanken bessere Leistung und Flexibilität bieten könnten. Jedoch bieten assoziative Arrays für die meisten typischen Einsatzfälle im Bereich der Clojure-Anwendungen ein robustes und effizientes Mittel zur Datenverwaltung.
