---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:00.358262-07:00
description: "Wie: C hat keine eingebaute Funktion f\xFCr die direkte Umwandlung von\
  \ Strings in Kleinbuchstaben, anders als einige Hochsprachen. Jedoch kann der Prozess\u2026"
lastmod: '2024-03-13T22:44:54.338244-06:00'
model: gpt-4-0125-preview
summary: "C hat keine eingebaute Funktion f\xFCr die direkte Umwandlung von Strings\
  \ in Kleinbuchstaben, anders als einige Hochsprachen."
title: Konvertierung eines Strings in Kleinbuchstaben
weight: 4
---

## Wie:
C hat keine eingebaute Funktion für die direkte Umwandlung von Strings in Kleinbuchstaben, anders als einige Hochsprachen. Jedoch kann der Prozess einfach unter Verwendung der Funktionen der C-Standardbibliothek implementiert werden. Unten findet sich eine Schritt-für-Schritt-Anleitung und ein Beispiel, das illustriert, wie man einen String in Kleinbuchstaben umwandelt.

```c
#include <stdio.h>
#include <ctype.h>

void toLowerCase(char *str) {
    while (*str) {
        *str = tolower(*str);
        str++;
    }
}

int main() {
    char text[] = "Hello, World!";
    printf("Original: %s\n", text);

    toLowerCase(text);
    printf("Lowercase: %s\n", text);

    return 0;
}
```

**Beispielausgabe:**

```
Original: Hello, World!
Lowercase: hello, world!
```

In diesem Beispiel iteriert die Funktion `toLowerCase` durch jedes Zeichen des Eingabestrings und wandelt es mit der Funktion `tolower` aus `ctype.h` in das entsprechende Kleinbuchstabenäquivalent um. Die Modifikation erfolgt direkt, wodurch der ursprüngliche String geändert wird.

## Tiefergehend
Die Funktion `tolower`, die im obigen Beispiel verwendet wird, ist Teil der C-Standardbibliothek, spezifisch innerhalb der Header-Datei `ctype.h`. Sie operiert basierend auf dem aktuellen Gebietsschema, aber für das Standard-"C" Lokale, behandelt sie den ASCII-Zeichensatz, wobei 'A' bis 'Z' zu 'a' bis 'z' konvertiert werden.

Historisch gesehen war die Handhabung der Zeichenkodierung und Fallumwandlung in C eng mit dem ASCII-Zeichensatz gekoppelt, was deren Nützlichkeit in internationalen oder lokalisierten Anwendungen einschränkte, bei denen Zeichen außerhalb des ASCII-Satzes häufig vorkommen. Moderne Programmiersprachen bieten möglicherweise eingebaute Stringmethoden, um Fallumwandlungen unter Berücksichtigung des Gebietsschemas und Unicode-Zeichen durchzuführen, was C nativ fehlt.

In Szenarien, die eine umfangreiche Textmanipulation erfordern, insbesondere mit nicht-ASCII-Zeichen, könnten Programmierer erwägen, Bibliotheken zu verwenden, die eine bessere Internationalisierungsunterstützung bieten, wie z.B. ICU (International Components for Unicode). Jedoch ist für die meisten Anwendungen, die mit ASCII-Text arbeiten, der gezeigte Ansatz effizient und unkompliziert. Er unterstreicht C's Neigung, Programmierern Kontrolle über die Datenmanipulation zu geben, wenn auch mit etwas mehr Aufwand im Vergleich zu höheren Programmiersprachen.
