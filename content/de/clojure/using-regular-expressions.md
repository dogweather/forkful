---
title:    "Clojure: Verwendung von regulären Ausdrücken"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Warum

Reguläre Ausdrücke sind ein mächtiges Werkzeug, das in vielen Programmiersprachen, einschließlich Clojure, verwendet werden kann. Sie ermöglichen es uns, spezifische Muster in Texten zu erkennen und zu manipulieren, was sehr nützlich ist, wenn wir große Datenmengen verarbeiten oder komplexe Vergleiche durchführen müssen. Daher ist es wichtig, die Grundlagen der regulären Ausdrücke zu verstehen, um effektiv mit Texten arbeiten zu können.

## Wie funktioniert es?

Die Verwendung von regulären Ausdrücken in Clojure ist einfach. Wir können die `re-seq` Funktion verwenden, um ein Textmuster in einer Zeichenkette zu finden und eine Liste mit übereinstimmenden Ergebnissen zurückzugeben. Zum Beispiel können wir eine Liste von Farben aus einem Text extrahieren:

```Clojure
"(Rot) (Grün) (Blau) (Gelb)"
(re-seq #"(\w+)" text)
```

Das Ergebnis dieser Funktion ist eine Liste mit den Farben "Rot", "Grün", "Blau" und "Gelb". Die `#"` und `"` Symbole geben an, dass es sich um einen regulären Ausdruck handelt. In diesem Fall suchen wir nach allen Buchstaben und Ziffern (durch `\w` dargestellt), die zwischen Klammern stehen.

## Tiefergehende Untersuchung

Um die Verwendung von regulären Ausdrücken in Clojure weiter zu vertiefen, können wir auch die `re-matches` Funktion verwenden, die eine Liste mit allen Übereinstimmungen zurückgibt, einschließlich der Teilausdrücke. Zum Beispiel können wir eine E-Mail-Adresse aus einem Text extrahieren und auch die Domain und die TLD separat speichern:

```Clojure
"E-Mail: admin@beispiel.com"
(re-matches #"(^.+)\@(.+)\.(.+)" text)
```

Das Ergebnis dieser Funktion ist eine Liste mit den Übereinstimmungen "admin@beispiel.com", "admin" und "beispiel" und "com". Das Zeichen `^` gibt an, dass wir am Anfang der Zeichenkette suchen möchten. Die Klammern um jeden Teil des regulären Ausdrucks ermöglichen es uns, die Übereinstimmungen separat abzurufen.

## Siehe auch

Hier sind einige zusätzliche Ressourcen, die Ihnen dabei helfen können, reguläre Ausdrücke in Clojure zu verstehen und effektiv anzuwenden:

- [Clojure-Dokumentation zu regulären Ausdrücken] (https://clojuredocs.org/clojure.core/re-seq)
- [Clojure Cheat Sheet zu regulären Ausdrücken] (https://clojure.org/api/cheatsheet#Regular%20Expressions)
- [Einführung in reguläre Ausdrücke in Clojure] (https://www.baeldung.com/regular-expressions-clojure)