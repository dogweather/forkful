---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Extrahieren von Teilstrings ist der Prozess, mit dem wir spezifische Teile einer Zeichenkette auswählen und isolieren. Es ist nützlich, wenn wir bestimmte Daten aus einem Textblock brauchen oder wenn wir das Verhalten unseres Programms auf bestimmte Wortgruppen oder -muster steuern möchten.

## So geht's:

Halten wir's einfach. Angenommen, Sie haben einen String und wollen einen Teil davon extrahieren. In C können wir das mit den Funktionen `strncpy()` und `sprintf()` tun. Hier ist ein Beispiel:

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char str[] = "Hallo, Welt!";
    char buff[50];

    strncpy(buff, str, 5);
    buff[5] = '\0'; // Fügt das Ende-Zeichen hinzu

    printf("Der Teilstring ist: %s\n", buff); // Gibt "Hallo" aus

    return 0;
}
```
Wichtig ist: Mit der Funktion `strncpy()` kopieren wir die ersten fünf Zeichen aus dem String in den Puffer. Dann setzen wir das Null-Zeichen manuell auf die Position nach dem letzten kopierten Zeichen, um das Ende des Strings zu markieren und nur den gewünschten Teil auszugeben.

## Deep Dive

Extrahieren von Teilstrings ist nichts Neues. Schon in den frühen Tagen von C standen Funktionen wie `strncpy()`, `strcat()`, `strstr()` zur Verfügung. Es gibt auch leistungsfähigere Alternativen wie `sscanf()`, `strchr()`, `strrchr()` usw. 

Aber Vorsicht: String-Manipulation kann zu gefürchteten Puffernüberläufen führen, wenn sie unsachgemäß verwendet wird. Im obigen Code kümmern wir uns darum, indem wir das Ende-Zeichen '\0' manuell setzen. Ohne dieses Zeichen würde `printf()` Bytes lesen, bis es ein Null-Zeichen findet, was einen Buffer-Überlauf verursachen kann.

## Siehe auch

Wollen Sie weiter gehen und C noch besser verstehen? Hier sind ein paar Links:

- Funktionen zur Zeichenkettenverarbeitung: https://en.cppreference.com/w/c/string/byte
- Sichere String-Manipulation in C: https://owasp.org/www-community/attacks/Buffer_overflow_attack
- Codewars zum Üben Ihres C-Codes: https://www.codewars.com/collections/basic-c