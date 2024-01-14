---
title:    "Arduino: Löschen von Zeichen mit übereinstimmendem Muster"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

Warum:

Die Arbeit mit Arduino bietet unzählige Möglichkeiten, kreative Projekte umzusetzen. Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann dabei hilfreich sein, um Daten korrekt zu verarbeiten oder unerwünschte Zeichen zu entfernen.

Wie geht das?

Die folgenden Beispiele zeigen, wie man mit Arduino Schritt für Schritt Zeichen löschen kann, die einem vorgegebenen Muster entsprechen. Wir werden dabei die Funktion "String.replace()" verwenden, die es ermöglicht, Zeichen innerhalb eines Strings zu ersetzen.

```Arduino
// Beispieldaten
String daten = "abcd_1234_xyz";
String zielmuster = "_";

// Zeichen löschen und neues Ergebnis ausgeben
daten.replace(zielmuster, "");
Serial.println(daten);  // Ausgabe: abcd1234xyz
```

Man sieht, dass das Zeichen "_" erfolgreich gelöscht wurde und nur noch die übrigen Zeichen in der Variablen "daten" enthalten sind.

Man kann aber auch mehrere Zeichen auf einmal löschen, indem man einfach mehrere Strings als Argumente in der Funktion "replace()" verwendet.

```Arduino
// Beispieldaten
String daten = "abc1f!g2h?i4j";
String zielmuster1 = "1";
String zielmuster2 = "!";
String zielmuster3 = "?";

// Zeichen löschen und neues Ergebnis ausgeben
daten.replace(zielmuster1, "");
daten.replace(zielmuster2, "");
daten.replace(zielmuster3, "");
Serial.println(daten);  // Ausgabe: abcfg2hij
```

Hier wurden die Zeichen "1", "!" und "?" angegeben und erfolgreich aus der Variablen "daten" gelöscht. Dies kann besonders nützlich sein, wenn man bestimmte Sonderzeichen entfernen möchte.

Tiefergehende Informationen:

Das Löschen von Zeichen kann auch mithilfe von regulären Ausdrücken (Regular Expressions) realisiert werden. Dabei können Muster angegeben werden, die eine bestimmte Gruppe von Zeichen ersetzen oder löschen. Dies bietet vielfältige Möglichkeiten und erfordert jedoch etwas mehr Kenntnisse über reguläre Ausdrücke.

```Arduino
// Beispieldaten
String daten = "abc1f!g2h?i4j";
String zielmuster = "[0-9!]";  // regulärer Ausdruck für Zahlen und Sonderzeichen

// Zeichen löschen und neues Ergebnis ausgeben
daten.replace(zielmuster, "");
Serial.println(daten);  // Ausgabe: abcfghij
```

Hier wird durch den regulären Ausdruck "[0-9!]" jede Zahl und jedes Sonderzeichen in der Variablen "daten" gelöscht. Dies ermöglicht es, noch gezielter und flexibler bestimmte Zeichen zu entfernen.

Siehe auch:

- [Arduino String Class Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino replace() Funktion](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Regular Expressions Tutorial](https://regexone.com/)