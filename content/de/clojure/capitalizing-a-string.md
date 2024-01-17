---
title:                "Eine Zeichenkette großschreiben"
html_title:           "Clojure: Eine Zeichenkette großschreiben"
simple_title:         "Eine Zeichenkette großschreiben"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Was & Warum?
Beim Steigern eines Strings handelt es sich um die Umwandlung des Anfangsbuchstabens jedes Wortes in Großbuchstaben. Programme tun dies, um eine konsistente Formatierung von Texten zu gewährleisten und die Lesbarkeit zu verbessern.

# So geht's:
```Clojure
(-> "hallo welt" 
    clojure.string/capitalize) 
;; Ausgabe: "Hallo Welt"
```

```Clojure
(-> "ich bin ein clojure programmierer" 
    (clojure.string/split #"\s") 
    (map clojure.string/capitalize) 
    (clojure.string/join " ")) 
;; Ausgabe: "Ich Bin Ein Clojure Programmierer"
```

# Tiefer Einblick:
Die Praxis des Steigerns von Strings ist nicht neu und wurde schon früher in anderen Programmiersprachen wie Python und Ruby verwendet. Alternativ kann auch die Funktion `upper-case` in Clojure genutzt werden, um alle Buchstaben groß zu schreiben. Die Methode `capitalize` wird unter der Haube mit `string.clj` implementiert, die den ersten Buchstaben eines Strings in Großbuchstaben konvertiert.

# Sieh auch:
Weitere Informationen zu String-Manipulationen in Clojure finden Sie in der offiziellen Dokumentation: https://clojure.org/guides/learn/string