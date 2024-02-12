---
title:                "Reguläre Ausdrücke verwenden"
aliases:
- /de/clojure/using-regular-expressions/
date:                  2024-02-03T19:16:29.189690-07:00
model:                 gpt-4-0125-preview
simple_title:         "Reguläre Ausdrücke verwenden"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke, ein mächtiges Werkzeug zur Mustervergleichung und Datenmanipulation, sind unverzichtbar bei Textverarbeitungsaufgaben wie Validierung der Eingabe, Suche und Ersetzung von Text. Programmierer nutzen sie umfangreich für komplexe String-Parsing- und Datenvalidierungsaufgaben auf effiziente und prägnante Weise.

## Wie geht das:
Clojure, seiner Wurzeln in der Lisp-Familie treu bleibend, bietet einen reichen Satz an Funktionen, die nahtlos mit den Fähigkeiten von Java in Bezug auf reguläre Ausdrücke interagieren. Hier ist, wie Sie diese nutzen können:

### Einfache Übereinstimmung
Um zu überprüfen, ob ein String einem Muster entspricht, verwenden Sie `re-matches`. Es gibt die gesamte Übereinstimmung zurück, wenn erfolgreich, oder `nil` andernfalls.

```clojure
(re-matches #"\d+" "123")  ;=> "123"
(re-matches #"\d+" "abc")  ;=> nil
```

### Nach Mustern suchen
Um das erste Vorkommen eines Musters zu finden, ist `re-find` Ihre Funktion der Wahl:

```clojure
(re-find #"\d+" "Bestellung 123")  ;=> "123"
```

### Erfassungsgruppen
Verwenden Sie `re-find` zusammen mit Klammern in Ihrem Muster, um Gruppen zu erfassen:

```clojure
(let [[_ area code] (re-find #"(1)?(\d{3})" "Telefon: 123-4567")]
  (println "Vorwahl:" area "Code:" code))
;; Ausgabe: Vorwahl: nil Code: 123
```

### Globale Suche (Alle Übereinstimmungen finden)
Clojure hat keine eingebaute globale Suche wie einige Sprachen. Verwenden Sie stattdessen `re-seq`, um eine faule Sequenz aller Übereinstimmungen zu erhalten:

```clojure
(re-seq #"\d+" "id: 123, Menge: 456")  ;=> ("123" "456")
```

### Strings teilen
Um einen String basierend auf einem Muster zu teilen, verwenden Sie `clojure.string/split`:

```clojure
(clojure.string/split "John,Doe,30" #",")  ;=> ["John" "Doe" "30"]
```

### Ersetzen
Ersetzen Sie Teile eines Strings, die einem Muster entsprechen, mit `clojure.string/replace`:

```clojure
(clojure.string/replace "2023-04-01" #"\d{4}" "JJJJ")  ;=> "JJJJ-04-01"
```

### Drittanbieter-Bibliotheken
Obwohl Clojure's eingebaute Unterstützung in den meisten Fällen ausreicht, sollten Sie für komplexere Szenarien Bibliotheken wie `clojure.spec` für robuste Datenvalidierung und `reagent` für reaktive DOM-Manipulation in Webanwendungen mit regex-basierter Routing- und Eingabevalidierung in Erwägung ziehen.

```clojure
;; Beispiel mit clojure.spec für die Validierung einer E-Mail
(require '[clojure.spec.alpha :as s])
(s/def ::email (s/and string? #(re-matches #".+@.+\..+" %)))
(s/valid? ::email "test@example.com")  ;=> wahr
```

Denken Sie daran, obwohl reguläre Ausdrücke mächtig sind, können sie auch den Code schwer lesbar und wartbar machen. Verwenden Sie sie mit Bedacht und ziehen Sie immer einfachere String-Manipulationsfunktionen wo möglich in Betracht.
