---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:00.358262-07:00
description: "Die Umwandlung eines Strings in Kleinbuchstaben in C bedeutet, alle\
  \ Gro\xDFbuchstaben in einem gegebenen String in ihre entsprechenden Kleinbuchstaben\
  \ zu\u2026"
lastmod: '2024-02-25T18:49:51.388784-07:00'
model: gpt-4-0125-preview
summary: "Die Umwandlung eines Strings in Kleinbuchstaben in C bedeutet, alle Gro\xDF\
  buchstaben in einem gegebenen String in ihre entsprechenden Kleinbuchstaben zu\u2026"
title: Konvertierung eines Strings in Kleinbuchstaben
---

{{< edit_this_page >}}

## Was & Warum?

Die Umwandlung eines Strings in Kleinbuchstaben in C bedeutet, alle Großbuchstaben in einem gegebenen String in ihre entsprechenden Kleinbuchstaben zu transformieren. Programmierer führen diese Operation oft durch, um Texteingaben für Vergleiche, Suchoperationen oder einfach für eine ästhetische Konsistenz in der Ausgabe zu standardisieren.

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
