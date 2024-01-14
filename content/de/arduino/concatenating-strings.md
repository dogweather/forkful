---
title:                "Arduino: Verketten von Zeichenfolgen"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Das Zusammenführen von Zeichenketten (auch bekannt als Konkatenation) ist eine wichtige Funktion in der Programmierung, die es ermöglicht, mehrere Zeichenketten zu einer zusammenzufügen. Dies kann hilfreich sein, um beispielsweise Texte dynamisch zu generieren oder Daten aus verschiedenen Quellen in einer Zeichenkette zu vereinen. In diesem Blogbeitrag erfährst du, wie du mit Arduino Zeichenketten concatenieren kannst.

## Wie geht's

Um Zeichenketten in Arduino zu konkatenieren, können wir die Funktion `concat()` verwenden. Diese Funktion nimmt mindestens zwei Parameter, die zu verbindenden Zeichenketten, und gibt eine neue Zeichenkette zurück, die die beiden ursprünglichen Zeichenketten beinhaltet.

```Arduino
#include <string.h>

void setup() {
  Serial.begin(9600);

  // zwei Zeichenketten definieren
  char string1[] = "Hallo, ";
  char string2[] = "welt!";

  // Zeichenketten konkatenieren
  char result[20];
  strcpy(result, string1);
  strcat(result, string2);

  // Ergebnis ausgeben
  Serial.println(result);
}

void loop() {

}
```

In diesem Beispiel verwenden wir `string.h`, um die Funktionen `strcpy()` und `strcat()` (von "string copy" und "string concatenation") zu nutzen. Zuerst kopieren wir die erste Zeichenkette in eine neue Zeichenkette namens `result` und verwenden dann `strcat()`, um die zweite Zeichenkette an `result` anzuhängen. Das Ergebnis wird dann über `Serial` ausgegeben.

Das könnte so aussehen:

```Arduino
Hallo, welt!
```

Natürlich können wir auch mehr als zwei Zeichenketten kombinieren, indem wir die `concat()`-Funktion mehrmals hintereinander aufrufen.

```Arduino
// drei Zeichenketten konkatenieren
char result[30];
strcpy(result, "Dies ist ");
strcat(result, "ein kleiner ");
strcat(result, "Satz.");

// Ausgabe: Dies ist ein kleiner Satz.
```

## Tiefer Einblick

Neben der `concat()`-Funktion gibt es noch andere Möglichkeiten, um Zeichenketten zu konkatenieren. Eine davon ist der sogenannte "Format String" oder "sprintf". Mit diesem können wir Platzhalter in einer Zeichenkette definieren und dann Werte an diesen Platzhalter einfügen.

```Arduino
char result[30];
sprintf(result, "Ich heiße %s und bin %d Jahre alt.", "Max", 25);

// Ausgabe: Ich heiße Max und bin 25 Jahre alt.
```

In diesem Beispiel wird `%s` durch den String "Max" und `%d` durch die Zahl 25 ersetzt. So können wir dynamische Zeichenketten erzeugen, zum Beispiel für die Ausgabe von Sensordaten.

## Siehe auch

- [String concatenation in C](https://www.programiz.com/c-programming/library-function/string.h/strcat)
- [Format strings in C](https://www.tutorialspoint.com/c_standard_library/c_function_sprintf.htm)

Danke fürs Lesen und viel Spaß beim Programmieren mit Arduino!