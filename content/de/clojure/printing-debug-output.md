---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Clojure: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

Was ist das & Warum?
Debug-Ausgaben sind Textnachrichten, die von Programmierern in ihrem Code platziert werden, um bei der Fehlerbehebung und Verfolgung von Programmabläufen zu helfen. Sie können eine nützliche Methode sein, um herauszufinden, was ein Programm zu einem bestimmten Zeitpunkt macht und wo mögliche Fehler auftreten könnten.

Wie geht das?
Es gibt verschiedene Möglichkeiten, Debug-Ausgaben in Clojure zu machen. Eine einfache Möglichkeit ist die Verwendung von ```(println "Debug message")```, um die gewünschte Nachricht auf der Konsole auszugeben. Eine andere Möglichkeit ist die Verwendung von Clojure's integrierter Logging-Bibliothek, ```clojure.tools.logging```, um genauere Steuerung über die Ausgabe zu haben.

Ein Beispiel mit println:
```Clojure
(defn add [a b]
  (println "Adding" a "and" b)
  (+ a b))

(add 2 3)
```

Dieses Beispiel wird die Ausgabe ```Adding 2 and 3``` produzieren.

Ein Beispiel mit Logging:
```Clojure
(require '[clojure.tools.logging :as log])

(defn subtract [a b]
  (log/debug "Subtracting" a "from" b)
  (- b a))

(subtract 5 2)
```

Dieses Beispiel wird die Debug-Nachricht ```Subtracting 2 from 5``` produzieren.

Tiefer tauchen
Debug-Ausgaben sind eine häufig verwendete Methode, um bei der Entwicklung von Software zu helfen. Früher war es üblich, sich dauerhaft auf Debug-Ausgaben zu verlassen, um Fehler zu finden. In der heutigen Zeit gibt es jedoch leistungsfähigere Tools wie Debugger und Testframeworks, die bei der Fehlerbehebung helfen können. Trotzdem kann es immer noch nützlich sein, Debug-Ausgaben zu machen, um zum Beispiel die Ausführungsreihenfolge von Funktionen zu überprüfen oder zu sehen, welche Werte bestimmte Variablen annehmen.

Eine Alternative zum Ausdrucken von Debug-Nachrichten ist das Einrichten eines Debuggers, der es einem ermöglicht, Schritt für Schritt durch den Code zu gehen und Variablenwerte anzuzeigen. Einige Entwickler bevorzugen auch die Verwendung von Clojure's dynamischen Typsystem, um während der Laufzeit Informationen über Variablen zu erhalten.

Siehe auch
- Die offizielle Dokumentation zu Clojure's Debugging-Tools: https://clojure.org/guides/tools_logging
- Eine Einführung in Clojure's dynamisches Typsystem: https://clojure.org/reference/types#dynvars