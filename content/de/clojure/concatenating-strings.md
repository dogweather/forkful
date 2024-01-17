---
title:                "Verbinden von Zeichenketten"
html_title:           "Clojure: Verbinden von Zeichenketten"
simple_title:         "Verbinden von Zeichenketten"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?
String-Konkatenation ist eine grundlegende Operation in der Programmierung, bei der mehrere Textabschnitte zu einem einzigen kombiniert werden. Programmierer nutzen dieses Konzept, um Texte zu erstellen, die spezifische Informationen enthalten, z.B. für die Ausgabe an Nutzer oder die Verwendung in Datenbankabfragen.

## Wie Geht's?
In Clojure kann die Konkatenation von Strings auf verschiedene Arten erfolgen. Eine Möglichkeit ist die ```str``` Funktion, die alle Argumente zu einem einzigen String zusammenfügt, wie im folgenden Beispiel:

```Clojure
(str "Hallo " "Welt") 
```
Dies wird die Ausgabe "Hallo Welt" erzeugen.

Eine andere Möglichkeit besteht darin, die "+" Operator zu verwenden, um zwei oder mehr Strings zusammenzufügen, wie im folgenden Beispiel:

```Clojure
(+ "Today is " "a good day")
```
Dies wird die Ausgabe "Today is a good day" erzeugen.

## Intensivtest
Die String-Konkatenation ist ein grundlegendes Konzept in vielen Programmiersprachen, nicht nur in Clojure. Es hat seine Wurzeln in der Textverarbeitung und wurde im Laufe der Zeit zu einem wichtigen Werkzeug für die Manipulation von Texten in der Programmierung.

In Clojure gibt es auch andere Möglichkeiten, Strings zu kombinieren, z.B. die ```StringBuilder``` Klasse, die effizienter bei der Manipulation von langen Strings ist. Es ist wichtig, die verschiedenen Optionen zu verstehen und die geeignetste Methode für die jeweilige Situation auszuwählen.

## Siehe Auch
Offizielle Clojure-Dokumentation zur ```str``` Funktion:
https://clojuredocs.org/clojure.core/str

Clojure Cookbook zum Thema String-Konkatenation:
https://github.com/clojure-cookbook/clojure-cookbook/blob/master/05_data/5-01_strings.asciidoc