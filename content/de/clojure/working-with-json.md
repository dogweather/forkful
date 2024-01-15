---
title:                "Arbeiten mit Json"
html_title:           "Clojure: Arbeiten mit Json"
simple_title:         "Arbeiten mit Json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit JSON befassen?

Die Antwort ist einfach: JSON ist ein beliebtes Format für den Austausch von Daten in Webanwendungen und APIs. Wenn man also mit Webentwicklung zu tun hat, ist es wichtig, sich mit JSON vertraut zu machen, um effektiv mit Daten umgehen zu können.

## Anleitung

Das erste, was du tun musst, um mit JSON in Clojure zu arbeiten, ist das Hinzufügen der Bibliothek `clojure.data.json` zu deinem Projekt. Dies kannst du ganz einfach mit dem `require` Befehl tun:

```Clojure
(require '[clojure.data.json :as json])
```

Als nächstes kannst du die `json/read` und `json/write` Funktionen verwenden, um Daten aus JSON-Dateien zu lesen oder JSON-Daten zu generieren. Zum Beispiel können wir eine JSON-Datei "data.json" erstellen, die folgendes enthält:

```json
{"name": "Max", "age": 25, "hobbies": ["Programming", "Hiking"]}
```

Um diese Daten in Clojure zu lesen, verwenden wir die `json/read` Funktion:

```Clojure
(json/read-str (slurp "data.json"))
;; => {:name "Max", :age 25, :hobbies ["Programming" "Hiking"]}
```

 Umgekehrt können wir auch Clojure-Daten in JSON umwandeln mit der `json/write` Funktion:

```Clojure
(json/write-str {:name "Max", :age 25, :hobbies ["Programming" "Hiking"]})
;; => "{\"name\":\"Max\",\"age\":25,\"hobbies\":[\"Programming\",\"Hiking\"]}"
```

Wie du siehst, gibt die `write-str` Funktion eine String-Repräsentation des JSON-Objekts aus.

Weitere Informationen zu den Funktionen `read` und `write` findest du in der [offiziellen Clojure-Dokumentation](https://clojure.github.io/data.json/).

## Tiefgründig

Nun möchtest du vielleicht mehr über den Umgang mit komplexen JSON-Strukturen erfahren. Zum Beispiel, wie man auf bestimmte Werte zugreift oder wie man JSON-Objekte verändert.

Zugriff auf Werte in einem JSON-Objekt können wir mit der `get-in` Funktion von Clojure machen, indem wir einfach den Schlüssel oder die Schlüssel, die wir auswählen möchten, als Argumente übergeben. Zum Beispiel können wir auf das Alter von Max zugreifen:

```Clojure
(get-in {:name "Max", :age 25, :hobbies ["Programming" "Hiking"]} [:age])
;; =>  25
```

Um ein JSON-Objekt zu verändern, gibt es verschiedene Ansätze, je nach Komplexität der Struktur. Eine einfache Möglichkeit ist es, die `assoc` Funktion zu verwenden und einen neuen Schlüssel-Wert-Paar hinzuzufügen:

```Clojure
(assoc {:name "Max", :age 25, :hobbies ["Programming" "Hiking"]} :location "Berlin")
;; => {:name "Max", :age 25, :hobbies ["Programming" "Hiking"], :location "Berlin"}
```

Andere Funktionen, die in diesem Zusammenhang nützlich sein können, sind `update`, `update-in` und `assoc-in`.

Für weitere Informationen über das Arbeiten mit JSON in Clojure empfehle ich dir, die [Clojure for the Brave and True](https://www.braveclojure.com/clojure-for-the-brave-and-true/) und [Clojure Cookbook](https://github.com/clojure-cookbook/clojure-cookbook) Bücher zu lesen.

## Siehe auch

- [Offizielle Clojure-Dokumentation über JSON](https://clojure.github.io/data.json/)
- [Clojure for the Brave and True Buch](https://www.braveclojure.com/clojure-for-the-brave-and-true/)
- [Clojure Cookbook](https://github.com/clojure-cookbook/clojure-cookbook)