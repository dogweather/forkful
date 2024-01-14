---
title:    "Arduino: Umwandeln eines Strings in Kleinbuchstaben"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Warum

Das Konvertieren von Text zu Kleinbuchstaben ist eine häufige Aufgabe beim Programmieren. Es kann nützlich sein, um Eingaben zu normalisieren oder Vergleichsoperationen durchzuführen.

# Wie geht es

Um eine Zeichenkette in Kleinbuchstaben zu konvertieren, können Sie die Funktion `toLowerCase()` verwenden. Diese Funktion ist Teil der Standardbibliothek von Arduino und kann auf Zeichenkettenobjekten angewendet werden. Hier ist ein Beispielcode:

```Arduino
String text = "Hallo WELT";
text.toLowerCase();
Serial.println(text);
```
Das obige Beispiel würde "hallo welt" auf der seriellen Konsole ausgeben. Sie können auch eine neue Zeichenkette erstellen, die die konvertierte Version enthält, wie in folgendem Beispiel:

```Arduino
String text = "Hallo WELT";
String new_text = text.toLowerCase();
Serial.println(new_text);
```

Die `toLowerCase()` Funktion ist nicht nur auf ASCII-Zeichen begrenzt, sondern funktioniert auch mit Unicode-Zeichen. Ein weiteres nützliches Feature ist die `toUpperCase()` Funktion, die eine Zeichenkette in Großbuchstaben konvertiert.

# Tiefergehende Einblicke

In der Programmierung von Zeichenketten gibt es verschiedene Konventionen für die Verwendung von Klein- oder Großbuchstaben. Das Konvertieren von Zeichenketten in eine bestimmte Schreibweise kann hilfreich sein, um Vergleiche durchzuführen oder bei der Verarbeitung von Eingaben konsistente Daten zu erhalten.

Eine wichtige Sache, die man beachten sollte, ist, dass das Konvertieren von Zeichenketten in Klein- oder Großbuchstaben die ursprüngliche Zeichenkette nicht ändert, sondern eine neue Zeichenkette erstellt. Wenn Sie also die konvertierte Zeichenkette speichern möchten, müssen Sie sie einem neuen Variablennamen zuweisen.

# Siehe auch

- [String-Methoden von Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [ASCII-Tabelle](https://www.ascii-code.com/) für die Übersicht der Zeichen und deren zugehörigen Codes.