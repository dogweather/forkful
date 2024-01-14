---
title:                "Clojure: Unterstrings extrahieren"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum
Das Extrahieren von Teilstrings kann eine nützliche Funktion in der Programmierung sein, um spezifische Informationen aus einem größeren String herauszufiltern. Dies kann hilfreich sein, wenn man beispielsweise eine Benutzereingabe analysiert oder bestimmte Daten aus einem Textdokument auslesen möchte.

## Anleitung
Die folgenden Codebeispiele zeigen, wie man mithilfe von Clojure Teilstrings extrahieren kann. Zunächst müssen wir die Funktion "subs" verwenden, die einen Teil eines Strings zurückgibt, basierend auf einem start- und end-Parameter. In diesem Beispiel extrahieren wir die ersten zwei Buchstaben aus dem String "Hallo".

```Clojure
(subs "Hallo" 0 2)
```

Output:
```Clojure
"Ha"
```

Man kann auch negative Indizes verwenden, um vom Ende des Strings aus zu zählen. In diesem Beispiel extrahieren wir den letzten Buchstaben aus dem String "Guten Tag".

```Clojure
(subs "Guten Tag" -1)
```

Output:
```Clojure
"g"
```

Mithilfe des "re-find" Befehls können wir auch mithilfe von regulären Ausdrücken Teilstrings extrahieren. In diesem Beispiel extrahieren wir alle Zahlen aus dem String "123abc456xyz".

```Clojure
(re-find #"\d+" "123abc456xyz")
```

Output:
```Clojure
"123" "456"
```

## Tiefere Einblicke
Es gibt zahlreiche Anwendungen für das Extrahieren von Teilstrings in der Programmierung. Zum Beispiel kann man damit auch komplexe Muster in Strings suchen und ersetzen, oder Daten aus Dateinamen auslesen.

Eine wichtige Sache, die man beim Extrahieren von Teilstrings beachten sollte, ist, dass die Indexierung in Clojure bei 0 beginnt. Das bedeutet, dass der erste Buchstabe im String den Index 0 hat und nicht 1, wie man es vielleicht gewohnt ist.

## Siehe auch
- [Offizielle Clojure Dokumentation zu subs](https://clojuredocs.org/clojure.core/subs)
- [Clojure String Manipulation Tutorial von Clojure for the Brave and True](https://www.braveclojure.com/working-with-strings/)
- [Regex Tutorial von Regular-Expressions.info](https://www.regular-expressions.info/tutorial.html)