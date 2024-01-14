---
title:    "Arduino: Löschen von Zeichen, die einem Muster entsprechen"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie ein Arduino-Entwickler sind, werden Sie sicherlich schon einmal auf die Aufgabe gestoßen sein, bestimmte Zeichenfolgen oder einzelne Zeichen aus einem Text oder einer Variablen zu löschen. Dies kann aus verschiedenen Gründen notwendig sein, zum Beispiel um unerwünschte Zeichen zu entfernen oder um Daten für die weitere Verarbeitung zu bereinigen. Hier erfahren Sie, wie Sie mit Ihrem Arduino ganz einfach Zeichen löschen können.

## Wie das geht

Um Zeichen zu löschen, die einem bestimmten Muster entsprechen, können Sie die `replace()`-Funktion verwenden. Sehen wir uns ein Beispiel an:

```Arduino
String text = "Hallo Arduino!";
text.replace("a", "");
Serial.println(text); // Ausgabe: Hllo Arduino!
```

Hier haben wir die `replace()`-Funktion verwendet, um alle Vorkommen von "a" in unserem Text zu ersetzen. Der zweite Parameter ist dabei leer, sodass die Zeichen einfach gelöscht werden. Beachten Sie, dass die `replace()`-Funktion immer nur das erste Vorkommen des Musters löscht. Um alle Vorkommen zu entfernen, müssen Sie diese Funktion in einer Schleife ausführen.

Natürlich können Sie auch ganze Wörter oder längere Zeichenfolgen löschen. Dazu einfach die zu löschenden Zeichen oder Wörter als Parameter in die Funktion einsetzen.

```Arduino
String text = "Hallo Arduino!";
text.replace("ll", "l");
Serial.println(text); // Ausgabe: Halo Arduino!
```

## Tiefere Einblicke

Um zu verstehen, wie die `replace()`-Funktion arbeitet, werfen wir einen Blick unter die Haube. Diese Funktion basiert auf der `indexOf()`-Funktion, die die Position des ersten Vorkommens eines Musters in einem Text zurückgibt. Ist das Muster nicht enthalten, wird `-1` zurückgegeben. Mit dieser Information kann die `replace()`-Funktion dann das entsprechende Zeichen löschen.

Wenn Sie mehr Kontrolle über die Löschung von Zeichen haben möchten, können Sie auch die `remove()`-Funktion verwenden. Diese erwartet als Parameter die Startposition und die Anzahl der zu löschenden Zeichen.

```Arduino
String text = "Hallo Arduino!";
text.remove(0, 5); // Entfernt die ersten 5 Zeichen
Serial.println(text); // Ausgabe: Arduino!
```

## Siehe auch

- `replace()`-Funktion: https://www.arduino.cc/reference/de/language/variables/data-types/stringobject/replace/
- `indexOf()`-Funktion: https://www.arduino.cc/reference/de/language/variables/data-types/stringobject/indexof/
- `remove()`-Funktion: https://www.arduino.cc/reference/de/language/variables/data-types/stringobject/remove/