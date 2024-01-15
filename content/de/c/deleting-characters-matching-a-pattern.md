---
title:                "Buchstaben löschen, die einem Muster entsprechen"
html_title:           "C: Buchstaben löschen, die einem Muster entsprechen"
simple_title:         "Buchstaben löschen, die einem Muster entsprechen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum
Manchmal kann es in der Programmierung notwendig sein, Zeichen zu löschen, die einem bestimmten Muster entsprechen. Dies kann zum Beispiel hilfreich sein, um unerwünschte Zeichen aus Benutzereingaben zu entfernen oder um Datenbereinigungsaufgaben durchzuführen.

## Wie geht das?
Um Zeichen in C zu löschen, die einem bestimmten Muster entsprechen, gibt es verschiedene Techniken, aber hier werden wir uns auf die Verwendung der Standardbibliotheksfunktion "memmove()" konzentrieren. Diese Funktion kopiert Bytes von einem Speicherort in einen anderen und überschreibt dabei den Zielbereich mit dem kopierten Inhalt. Wir können diese Funktion nutzen, um die unerwünschten Zeichen zu löschen.

```C
#include <stdio.h>
#include <string.h>

void delete_characters(char *str, char pattern) {
    int i, j;
    // Schleife über alle Zeichen im String
    for (i = 0, j = 0; str[i] != '\0'; i++) {
        // Wenn das aktuelle Zeichen nicht dem Muster entspricht,
        // kopiere es in den String an der nächsten Position
        if (str[i] != pattern) {
            str[j++] = str[i];
        }
    }
    // Setze das Ende des Strings
    str[j] = '\0';
}

int main() {
    char str[] = "Hallo Welt!";
    // Lösche alle Leerzeichen aus dem String
    delete_characters(str, ' ');
    printf("%s", str); // Ausgabe: "HalloWelt!"
    return 0;
}
```

## Tiefergehende Informationen
Der Code in der "delete_characters()" Funktion kann auch auf komplexere Weise angepasst werden, um beispielsweise mehrere Zeichen gleichzeitig zu löschen oder eine dynamische Mustererkennung zu ermöglichen. Dabei bietet das Verständnis der Arbeit von "memmove()" und wie sie mit Pointer- und Speicherarbeit zusammenhängt eine Möglichkeit, verschiedene effiziente Implementierungen zu designen.

## Siehe auch
- [memmove() Dokumentation](https://www.tutorialspoint.com/c_standard_library/c_function_memmove.htm)
- [C String Manipulation Guide](https://hackr.io/tutorials/learn-c-string-manipulation)