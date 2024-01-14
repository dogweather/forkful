---
title:                "Clojure: Debug-Ausgabe drucken"
simple_title:         "Debug-Ausgabe drucken"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

# Warum

Debugging ist ein wichtiger Teil des Programmierens und das Ausgeben von Debug-Informationen ist eine effektive Methode, um Fehler in Ihrem Code zu finden. Es kann auch hilfreich sein, um das Verhalten Ihres Codes zu verstehen und zu überprüfen, ob die erwarteten Ergebnisse erzielt werden.

# Wie geht man vor

Das Drucken von Debug-Informationen in Clojure ist einfach und kann auf verschiedene Arten erfolgen. Eines der gebräuchlichsten Methoden ist die Verwendung von `println`, um eine Nachricht in der Konsole auszugeben. Zum Beispiel:

```Clojure
(println "Dies ist eine Debug-Nachricht.")
```

Dies wird die Nachricht "Dies ist eine Debug-Nachricht." in der Konsolenansicht ausgeben.

Eine weitere Möglichkeit ist die Verwendung von `prn`, um eine lesbarere Ausgabe zu erzeugen. Zum Beispiel:

```Clojure
(def my-map {:name "Max" :age 30})
(prn my-map)
```

Dies wird das Datum in einer geordneteren und lesbareren Form ausgeben:

```Clojure
{:name "Max" :age 30}
```

Sie können auch Spezialformen wie `tap>` verwenden, um gezielt Debug-Informationen auszugeben. Dieses Beispiel zeigt, wie Sie eine Variable überwachen können, um sicherzustellen, dass sie den erwarteten Wert enthält:

```Clojure
(def my-var 10)
(def my-var-tap (tap> my-var (println "Meine Variable hat jetzt den Wert:")))
```

Dieser Code wird jedes Mal, wenn `my-var` geändert wird, die aktuelle Nummer in der Konsole ausgeben.

# Tiefer eintauchen

Erfahrene Clojure-Entwickler können auch `clojure.spec` verwenden, um Debug-Informationen auszugeben. `clojure.spec` ist eine Funktion, die es Ihnen ermöglicht, spezifische Regeln und Überprüfungen für Ihre Datenstrukturen zu definieren. Die `explain`-Funktion kann verwendet werden, um detaillierte Informationen über die Werte zurückzugeben, die nicht den Spezifikationen entsprechen. Zum Beispiel:

```Clojure
(require '[clojure.spec.alpha :as s])

(s/def ::name string?)
(s/def ::age int?)

(def my-map {:name "Max" :age "30"}) ; beachten Sie den falschen Datentyp für :age

(s/explain ::person my-map)
```

Die Ausgabe wird Folgendes beinhalten:

```Clojure
{:name "Max", :age "30"}
% in: [:age] val: "30" fails spec
```

Dies kann Ihnen helfen, schnell zu identifizieren, welche spezifischen Werte nicht den Erwartungen entsprechen und Ihnen helfen, die Ursache für Fehler in Ihrem Code herauszufinden.

# Siehe auch

- Offizielle Clojure Dokumentation zu debuggen (https://clojure.org/guides/debugging)
- Clojure Cheatsheet (https://clojure.org/api/cheatsheet)
- LearnXinY-Materialien zur Fehlersuche (https://www.learnxinyminutes.com/docs/clojure/)