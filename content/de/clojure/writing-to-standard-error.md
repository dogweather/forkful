---
title:                "Schreiben auf die Standardfehlerausgabe"
html_title:           "Clojure: Schreiben auf die Standardfehlerausgabe"
simple_title:         "Schreiben auf die Standardfehlerausgabe"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man überhaupt Texte in das Standardfehlerausgabefenster schreiben? Nun, es gibt verschiedene Gründe dafür. Zum Beispiel kann es hilfreich sein, Fehlermeldungen zu debuggen oder generelle Statusmeldungen während der Ausführung eines Programms zu überwachen. Es ist auch eine gute Möglichkeit, Dinge zu protokollieren, die nicht in die reguläre Ausgabe gehören, aber dennoch wichtig sind.

## Wie geht das?

Um Text in das Standardfehlerausgabefenster zu schreiben, gibt es in Clojure die Funktion `(println & xs)`. Diese Funktion nimmt beliebig viele Argumente entgegen und gibt diese nacheinander in das Standardfehlerausgabefenster aus. Hier ein Beispiel:

```Clojure
(println "Dies ist ein Beispiel")
(println "für das Schreiben" "in das")
(println "Standardfehlerausgabefenster.")
```

Das obige Beispiel würde folgende Ausgabe in das Standardfehlerausgabefenster schreiben:

```
Dies ist ein Beispiel
für das Schreiben in das
Standardfehlerausgabefenster.
```

## Tiefergehende Informationen

Das Schreiben in das Standardfehlerausgabefenster kann besonders nützlich sein, wenn man mit mehreren Threads oder Prozessen arbeitet, da die Ausgaben unabhängig voneinander auftauchen und nicht vermischt werden. Außerdem ist es eine gute Möglichkeit, um die Ursache von Fehlern oder unerwartetem Verhalten schnell zu finden. 

Eine Sache, die man beachten sollte, ist, dass das Standardfehlerausgabefenster normalerweise nur während der Ausführung eines Programms sichtbar ist und nicht dauerhaft gespeichert wird. Deshalb ist es ratsam, eine Log-Datei zu erstellen, in die man wichtige Informationen zusätzlich schreibt.

## Siehe auch

- [Offizielle Dokumentation von Clojure](https://clojure.org/)
- [Ein Tutorial zum Einstieg in Clojure](https://www.tutorialspoint.com/clojure/index.htm)