---
title:                "Fehlerbehandlung"
date:                  2024-01-26T00:51:59.026553-07:00
model:                 gpt-4-1106-preview
simple_title:         "Fehlerbehandlung"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/handling-errors.md"
---

{{< edit_this_page >}}

## Was & Warum?
Fehlerbehandlung geht darum, das Unerwartete in Programmen zu managen – wie ein Türsteher, der sich um Unruhestifter kümmert. Programmierer mögen es reibungslos; Fehlerbehandlung hilft dabei, die Problemfälle in Schach zu halten, um sicherzustellen, dass ihr Code nicht stolpert und fällt, wenn er mit dem Unerwarteten konfrontiert wird.

## Wie geht das:
Clojure, wie seine Lisp-Vorfahren, stützt sich auf Ausnahmen (Exceptions), um mit Fehlern umzugehen. Hier ist, wie du dein Können zeigst, wenn die Dinge schiefgehen.

Eine Ausnahme zu werfen ist einfach:
```Clojure
(throw (Exception. "Hoppla! Etwas ist schief gelaufen."))
```

Eine Ausnahme zu fangen, das wirst du oft tun:
```Clojure
(try
  ;; riskanter Code
  (/ 1 0)
  (catch ArithmeticException e
    (println "Division durch null nicht möglich!"))
  ;; finally Block wird so oder so ausgeführt
  (finally 
    (println "Hier kommt der Aufräum-Code.")))
```
Beispiel-Ausgabe für den obigen catch-Block:
```
Division durch null nicht möglich!
Hier kommt der Aufräum-Code.
```

`ex-info` und `ex-data` nutzen, um reichhaltigere Kontextinformationen über Ausnahmen zu bekommen:
```Clojure
(try
  ;; Auslösen einer benutzerdefinierten Ausnahme
  (throw (ex-info "Benutzerdefinierter Fehler" {:type :custom-failure}))
  (catch Exception e
    ;; die Daten aus unserer benutzerdefinierten Ausnahme holen
    (println (ex-data e))))
```
Beispiel-Ausgabe:
```
{:type :custom-failure}
```

## Tiefergehend
Die Geschichte der Fehlerbehandlung in Clojure ist nicht radikal anders als die anderer Lisps oder sogar Java (von dem es den `try-catch`-Mechanismus erbt). Es ist pragmatisch; Ausnahmen zu verwenden ist der Hauptweg, genau wie in Java, aber Clojure bietet eine funktionale Note mit `ex-info` und `ex-data` für detailliertere Fehlerdaten.

Alternativen für Fehlerbehandlung in Clojure beinhalten die Verwendung monadischer Konstrukte, wie die `either` Monad aus Bibliotheken wie `cats`, oder core.async für kanalbasierte Fehlerfortpflanzung. Allerdings sind diese komplexer und werden in spezifischen Szenarien verwendet.

Historisch gesehen hat sich die Fehlerbehandlung in Programmiersprachen von einfachen Statusrückgaben zu den ausgefeilteren Ausnahmebehandlungsmechanismen moderner Sprachen entwickelt. Clojure entscheidet sich für Einfachheit und eine Prise funktionale Programmierung, indem es Altes und Neues vermischt.

## Siehe auch
- Clojures Anleitung zu Ausnahmen: https://clojure.org/guides/exceptions
- „Cats“-Bibliothek für funktionalere Ansätze: https://github.com/funcool/cats
- „Core.async“ für asynchrone Programmierung: https://github.com/clojure/core.async
