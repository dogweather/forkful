---
title:                "C: Textsuche und -ersetzung"
simple_title:         "Textsuche und -ersetzung"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum wir uns mit der Suche und Ersetzung von Text beschäftigen: Wir wollen zum Beispiel Fehler in unserem Code beheben, bestimmte Wörter oder Ausdrücke in unseren Texten ändern oder einfach nur die Formatierung eines Dokuments anpassen. Mit C-Programmierung können wir schnell und effizient Text durchsuchen und ersetzen, was uns viel Zeit und Aufwand ersparen kann.

## So geht's

Um Text in C zu suchen und zu ersetzen, können wir die Funktionen `strstr()` und `strreplace()` verwenden. Die `strstr()` Funktion durchsucht einen angegebenen Text nach einem bestimmten Substring und gibt ein Pointer auf die Position des gefundenen Substrings zurück. Die `strreplace()` Funktion ersetzt dann den gefundenen Substring durch einen anderen angegebenen Text. Hier ist ein Beispiel, wie wir diese Funktionen nutzen können:

```C
#include <stdio.h>
#include <string.h>

int main() {

	// Erstelle einen Text als Beispiel
	char text[100] = "Willkommen auf meinem Blog!";

	// Suche nach dem Substring "Blog"
	char *found = strstr(text, "Blog");

	// Ersetze "Blog" durch "Website"
	strreplace(text, found, "Website");

	// Gib den veränderten Text aus
	printf("%s\n", text);

	return 0;
}
```

Das obige Beispiel gibt folgende Ausgabe aus: "Willkommen auf meiner Website!". Wir können auch mehrere `strreplace()` Funktionen hintereinander nutzen, um mehrere Textänderungen auf einmal vorzunehmen.

## Tiefere Einblicke

Die `strstr()` und `strreplace()` Funktionen sind in der Standardbibliothek von C enthalten. Sie sind Teil der `string.h` Header-Datei, die viele nützliche Funktionen für die Verarbeitung von Strings bereitstellt. Wenn wir genauer verstehen wollen, wie diese Funktionen arbeiten, können wir uns die Quelltexte in der Offenen C Standardbibliothek (glibc) ansehen.

Es gibt auch viele andere Möglichkeiten, Text in C zu suchen und zu ersetzen, wie zum Beispiel die Verwendung von regulären Ausdrücken oder benutzerdefinierten Suchalgorithmen. Mit etwas Übung und Erfahrung können wir herausfinden, welcher Ansatz am besten für unsere spezifische Aufgabe geeignet ist.

## Siehe auch

- [C-Programmierung auf Wikibooks](https://de.wikibooks.org/wiki/C-Programmierung)
- [Offene C Standardbibliothek auf GitHub](https://github.com/bminor/glibc)