---
title:                "Lese Befehlszeilenargumente"
html_title:           "Javascript: Lese Befehlszeilenargumente"
simple_title:         "Lese Befehlszeilenargumente"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Was & Warum?
Das Lesen von Befehlszeilenargumenten ist eine Möglichkeit für Programmierer, externe Eingaben in ihr Skript einzubinden. Dies kann hilfreich sein, um Benutzereingaben oder Konfigurationsparameter direkt von der Kommandozeile aus abzurufen.

Wie geht's?
In Javascript können Befehlszeilenargumente über das globale Objekt "process" gelesen werden. Das Argument-Array kann mit "process.argv" abgerufen werden. Hier ist ein Beispielcode, der das zweite Argument ausgibt:
```Javascript
const argument = process.argv[2];
console.log(argument);
```
Wenn wir diesen Code mit dem Befehl "node script.js hello" ausführen, wird "hello" als Output zurückgegeben.

Deep Dive
Das Lesen von Befehlszeilenargumenten ist keine neue Funktion in der Programmierung. Es wurde bereits in älteren Programmiersprachen wie C oder Bash verwendet. Auch in anderen Sprachen wie Python oder Java gibt es ähnliche Möglichkeiten, Befehlszeilenargumente zu lesen. Alternativ können auch Benutzereingaben über eine grafische Benutzeroberfläche abgefragt werden.

Die Implementierung von Befehlszeilenargumenten in Javascript ist relativ einfach und kann in vielen Anwendungsfällen nützlich sein. Es ist jedoch wichtig zu beachten, dass alle übergebenen Argumente als Zeichenketten interpretiert werden und zuerst auf den entsprechenden Datentyp konvertiert werden müssen. Auch die Validierung der Eingaben ist in der Verantwortung des Programmierers.

See Also
Für weitere Informationen und Beispiele zum Lesen von Befehlszeilenargumenten in Javascript empfehlen wir folgende Quellen:
- Die offizielle Dokumentation von Node.js zum Thema "process" und "process.argv"
- Eine kurze Einführung in Befehlszeilenargumente in Javascript auf dem Blog von freecodecamp.org
- Ein ausführliches Tutorial über die Verwendung von process.argv auf Medium.com