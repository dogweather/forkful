---
title:                "Anführungszeichen aus einem String entfernen"
date:                  2024-01-26T03:37:48.739637-07:00
model:                 gpt-4-0125-preview
simple_title:         "Anführungszeichen aus einem String entfernen"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Entfernen von Anführungszeichen aus einem String bedeutet, jegliche Anführungszeichen—sei es einfache ('') oder doppelte ("")—, die Teil des Inhalts des Strings sind, herauszufiltern. Programmierer tun dies, um Eingaben zu bereinigen, Daten für die weitere Verarbeitung vorzubereiten oder Syntaxfehler zu vermeiden, wenn sie mit Dateipfaden und Befehlen in Sprachen arbeiten, die Anführungszeichen verwenden, um Strings abzugrenzen.

## Wie:

Hier ist eine C-Funktion, die diese lästigen Anführungszeichen aus Ihren Strings entfernt:

```c
#include <stdio.h>
#include <string.h>

void remove_quotes(char *str) {
    char *p_read = str, *p_write = str;
    while (*p_read) {
        if (*p_read != '"' && *p_read != '\'') {
            *p_write++ = *p_read;
        }
        p_read++;
    }
    *p_write = '\0';
}

int main() {
    char str[] = "He said, \"Hello, 'world'!\"";
    printf("Original: %s\n", str);
    remove_quotes(str);
    printf("Bereinigt: %s\n", str);
    return 0;
}
```

Beispielausgabe:

```
Original: He said, "Hello, 'world'!"
Bereinigt: He said, Hello, world!
```

## Tiefere Betrachtung

Das Entfernen von Anführungszeichen aus einem String ist eine Aufgabe, die es seit den Anfängen der Programmierung gibt, bei der die Datenhygiene der Schlüssel zum Vermeiden von Fehlern (wie SQL-Injection-Angriffen) war und ist oder um sicherzustellen, dass ein String sicher an Systeme übergeben werden kann, die ein Anführungszeichen für ein Steuerzeichen halten könnten.

Historisch gesehen, behandeln verschiedene Sprachen diese Aufgabe unterschiedlich—einige haben eingebaute Funktionen (wie `strip` in Python), während andere, wie C, manuelle Implementierung erfordern, aufgrund ihres Schwerpunkts auf der Gewährung von Entwicklern niedrigstufiger Kontrolle.

Alternativen beinhalten die Verwendung von Bibliotheksfunktionen wie `strpbrk`, um Anführungszeichen zu finden oder reguläre Ausdrücke (mit Bibliotheken wie PCRE) für komplexere Muster zu verwenden, obwohl dies für das bloße Entfernen von Anführungszeichen übertrieben sein könnte.

Die obige Implementierung scannt einfach jeden Charakter im String und kopiert nur die Nicht-Anführungszeichen-Charaktere an die Schreibzeigerposition. Dies ist effizient, da es ohne zusätzlichen Speicherbedarf für den Ergebnisstring vor Ort erfolgt.

## Siehe auch

- [C-Standardbibliotheksfunktionen](http://www.cplusplus.com/reference/clibrary/)
- [PCRE - Perl Compatible Regular Expressions](https://www.pcre.org/)
- [Verstehen von Zeigern in C](https://www.learn-c.org/en/Pointers)
