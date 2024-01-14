---
title:    "Clojure: Die Länge eines Strings ermitteln"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Warum

Das Finden der Länge eines Strings ist eine grundlegende Fähigkeit beim Programmieren. Es ermöglicht uns, die Größe einer Zeichenkette zu bestimmen und sie je nach Bedarf zu verarbeiten.

# Wie geht man vor?

Das Finden der Länge eines Strings in Clojure ist einfach und unkompliziert. Hier sind ein paar Beispiele, die dir zeigen, wie du das machen kannst:

```Clojure
; Definiere einen String
(def my-string "Hallo Welt")

; Verwende die Funktion `count` um die Länge des Strings zu finden
(count my-string)
; Output: 10

; Kombiniere `count` mit `format`, um eine Nachricht mit der Länge des Strings zu erstellen
(format "Die Länge des Strings beträgt %d" (count my-string))
; Output: "Die Länge des Strings beträgt 10"
```

# Tiefer tauchen

Bei der Verwendung von `count` sollte beachtet werden, dass sie auch für andere Datentypen wie Vektoren und Maps funktioniert. Außerdem gibt `count` die Anzahl der Elemente in einem String oder einer Datenstruktur zurück, und nicht die Anzahl der tatsächlichen Buchstaben. Dies liegt daran, dass in Clojure Strings als Sequenzen von einzelnen Zeichen behandelt werden.

Hier sind ein paar weitere Beispiele, um das Konzept der Länge in Clojure besser zu verstehen:

```Clojure
(count ["Hello" "World"])
; Output: 2

(count {:a 1 :b 2 :c 3})
; Output: 3
```

Es ist auch wichtig zu beachten, dass `count` eine sehr effiziente Funktion ist und schnell auf große Datenstrukturen angewendet werden kann.

# Siehe auch

- Offizielle Dokumentation von Clojure zu `count`: https://clojuredocs.org/clojure.core/count
- Clojure String-Funktionen: https://clojuredocs.org/quickref
- Eine Liste von hilfreichen Funktionen zum Verarbeiten von Strings in Clojure: https://www.lispcast.com/8-handy-clojure-functions-for-string-manipulation