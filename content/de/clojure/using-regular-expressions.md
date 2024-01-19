---
title:                "Reguläre Ausdrücke verwenden"
html_title:           "Bash: Reguläre Ausdrücke verwenden"
simple_title:         "Reguläre Ausdrücke verwenden"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?

Regular Expressions (RegEx) sind Muster, die helfen, Zeichenketten darzustellen und zu suchen. Programmierer nutzen sie, um effizienter Code zu schreiben und wiederholte Muster in Strings zu finden oder zu ersetzen.

## Wie geht das:

Mit Clojure können wir RegEx nutzen, um komplexe Such- und Ersetzungsvorgänge auszuführen. Hier ist ein einfaches Beispiel:

```Clojure
(re-seq #"\w+" "Hallo, Welt!")
```

Dieses Code-Schnipsel findet alle Worte in dem gegebenen String. Die Ausgabe sieht so aus:

```Clojure
("Hallo" "Welt")
```

## Tiefer Eintauchen:

RegEx gibt es schon lange. In den 1950ern entwickelten Wissenschaftler sie, um die Verarbeitung von Zeichenketten zu verbessern. In Clojure verwenden wir den #"..." Syntax für RegEx, der bindungslos ist.

Alternativen zu RegEx sind Tools wie Texteditoren oder Shell-Skripte, aber wegen der Macht und Flexibilität von RegEx sind sie oft die erste Wahl. Trotzdem, bei sehr komplexen Mustern kann RegEx unübersichtlich und schwierig zu warten sein.

Die Implementierung von RegEx in Clojure basiert auf der Java Struktivierung. Weil Java RegEx auch unterstützt, kann Clojure auf dieses mächtige Werkzeug zugreifen und seine Vorteile nutzen.

## Siehe auch:

Für weitere Informationen, hier einige Links:

- Clojure Doku: https://clojure.org/guides/weird_characters
- Java RegEx: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/regex/Pattern.html
- Praktischer Leitfaden für RegEx: https://www.regular-expressions.info/tutorial.html