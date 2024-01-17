---
title:                "Suchen und Ersetzen von Text"
html_title:           "Java: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Was & Warum?
Wenn du als Programmierer mit Text arbeitest, wirst du irgendwann auf die Notwendigkeit stoßen, Teile davon zu ändern oder zu ersetzen. Das nennt sich "Suchen und Ersetzen" und ist eine wesentliche Fähigkeit, die dir helfen wird, effizienter zu arbeiten und Zeit zu sparen. 

Warum machen wir das? Nun, es gibt verschiedene Gründe: vielleicht möchtest du bestimmte Wörter durch andere ersetzen, um den Code lesbarer zu machen oder um Fehler zu korrigieren. Oder du möchtest wiederkehrende Textpassagen automatisch ändern, um deine Arbeit zu vereinfachen. Egal aus welchem Grund, die Suchen-und-Ersetzen-Funktion ist ein unverzichtbares Tool für jeden Programmierer.

# Wie geht's?
In Java gibt es mehrere Möglichkeiten, nach bestimmten Texten zu suchen und sie zu ersetzen. Ein einfacher Weg ist die Verwendung der Methode `replace()` der Klasse `String`. Hier ist ein Beispiel, um das Wort "Hallo" durch "Hi" zu ersetzen:

```Java
String text = "Hallo Welt!";
String newText = text.replace("Hallo", "Hi");
System.out.println(newText);
```

Das Programm wird "Hi Welt!" ausgeben. Alternativ kannst du auch den `replaceAll()` Befehl verwenden, um alle Vorkommen eines bestimmten Patterns zu ersetzen.

Um bestimmte Textpassagen zu suchen, kannst du auch reguläre Ausdrücke verwenden. Die Klasse `Pattern` bietet hierfür die nötigen Methoden. Hier ist ein Beispiel, um alle Zahlen aus einem String zu entfernen:

```Java
String text = "123 Java ist cool";
String newText = text.replaceAll("\\d", ""); // regulärer Ausdruck für Zahlen
System.out.println(newText);
```

Das Programm wird "Java ist cool" ausgeben.

# Tiefer tauchen
Suchen und Ersetzen ist keine neue Idee und wurde schon lange vor der Entstehung von Java verwendet. In früheren Programmiersprachen wurde oft der Begriff "Substitution" verwendet, um das Ersetzen von Text zu beschreiben.

Es gibt auch verschiedene Tools und Texteditoren, die die Suche und Ersetzen-Funktion anbieten, wie zum Beispiel Notepad++ oder Sublime Text.

In Java ist es auch möglich, eigene Suchen-und-Ersetzen-Funktionen zu implementieren. Du kannst zum Beispiel eine Klasse erstellen, die eine bestimmte Datei nach einem bestimmten Pattern durchsucht und es durch einen anderen Text ersetzt. Dies kann besonders nützlich sein, wenn du mit großen Text-Dateien arbeitest.

Egal wie du Suchen und Ersetzen verwendest, es ist wichtig, dass du verstehst, dass es immer auf einen bestimmten Teil des Textes angewendet wird und nicht automatisch alle Vorkommen im gesamten Programm ersetzt.

# Siehe auch
Es gibt noch viele weitere Funktionen und Methoden in Java, um Texte zu verarbeiten. Einige davon sind `substring()` zum Extrahieren von Teilstrings, `trim()` zum Entfernen von Leerzeichen am Anfang und Ende eines Strings, und `toUpperCase()` und `toLowerCase()` zum Ändern der Groß- und Kleinschreibung.

Mehr Informationen und Beispiele dazu findest du in der [offiziellen Java-Dokumentation](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html).