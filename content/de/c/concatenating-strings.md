---
title:    "C: Verkettung von Zeichenketten"
keywords: ["C"]
---

{{< edit_this_page >}}

# Warum sich mit dem Verketten von Strings beschäftigen?

Das Verketten von Strings ist ein grundlegender Teil der Programmierung in C und kann in vielen Anwendungsfällen nützlich sein. Es ermöglicht es uns, mehrere Zeichenfolgen miteinander zu verbinden und so komplexe Ausgaben zu erzeugen. Obwohl es auf den ersten Blick einfach erscheinen mag, gibt es einige wichtige Konzepte zu beachten, um die besten Ergebnisse zu erzielen.

# Wie man Strings in C verketten kann

Um Strings in C zu verketten, können wir die Funktion "strcat" verwenden. Diese Funktion nimmt zwei Strings als Argumente und verbindet sie miteinander, wobei der zweite String an den ersten angehängt wird. Um es zu nutzen, müssen wir die Bibliothek "string.h" in unserem Code einbinden.

```C
#include <stdio.h>
#include <string.h>

int main(void) {
  // Initialisieren der Strings
  char str1[20] = "Hallo";
  char str2[] = "Welt";

  // Verketten der Strings
  strcat(str1, str2);

  // Ausgabe des Ergebnisses
  printf("%s", str1);

  return 0;
}
```

Die Ausgabe dieses Codeschnipsels wäre "HalloWelt".

Neben der Funktion "strcat" gibt es auch noch andere Möglichkeiten, Strings zu verketten, wie zum Beispiel die Verwendung von Schleifen und Zeigern. Es ist wichtig, verschiedene Methoden zu erlernen und zu verstehen, um das Beste aus unseren Programmen herausholen zu können.

# Tiefergehende Informationen über das Verketten von Strings

Bei der Verknüpfung von Strings müssen wir uns bewusst sein, dass der erste String groß genug sein muss, um den zweiten aufnehmen zu können. Andernfalls kann es zu unerwünschten Ergebnissen führen, wie zum Beispiel das Überschreiben von Speicherbereichen. Es ist auch wichtig zu beachten, dass String-Arrays immer mit einem Nullterminator enden müssen, damit sie als gültige C-Strings behandelt werden.

Es gibt auch Fälle, in denen wir mehrere Zeichenfolgen miteinander verbinden müssen, anstatt nur zwei. Für diesen Zweck können wir die Funktion "strcat" in einer Schleife verwenden, um nacheinander mehrere Strings zu verketten.

# Siehe auch

- [C-Programmierung für Anfänger](https://www.guru99.com/c-programming-for-beginners.html)
- [Offizielle C-Dokumentation](https://devdocs.io/c/)
- [Stringverarbeitung in C](https://www.studytonight.com/c/string-processing.php)