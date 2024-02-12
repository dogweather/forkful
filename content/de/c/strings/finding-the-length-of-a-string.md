---
title:                "Die Länge einer Zeichenkette ermitteln"
aliases:
- /de/c/finding-the-length-of-a-string.md
date:                  2024-02-03T17:56:23.691369-07:00
model:                 gpt-4-0125-preview
simple_title:         "Die Länge einer Zeichenkette ermitteln"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/finding-the-length-of-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Die Ermittlung der Länge eines Strings in C beinhaltet die Bestimmung der Anzahl der Zeichen vor dem Nullterminator `\0`. Programmierer tun dies, um Strings korrekt manipulieren zu können, ohne auf Fehler wie Pufferüberläufe zu stoßen, die zu Sicherheitsanfälligkeiten oder Programmabstürzen führen können.

## Wie geht das:
In C wird die Standardbibliotheksfunktion `strlen()` häufig verwendet, um die Länge eines Strings zu ermitteln. Hier ist ein schnelles Beispiel:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char myString[] = "Hello, World!";
    size_t length = strlen(myString);
    
    printf("Länge von '%s' ist %zu.\n", myString, length);
    
    return 0;
}
```

**Beispielausgabe:**
```
Länge von 'Hello, World!' ist 13.
```

In diesem Beispiel nimmt `strlen()` einen String (`myString`) als Eingabe und gibt seine Länge ohne den Nullterminator zurück. Die Verwendung von `size_t` für die Längenvariable wird empfohlen, da es sich um einen vorzeichenlosen Ganzzahltyp handelt, der in der Lage ist, die Größe des größtmöglichen Objekts im System darzustellen.

## Tiefergehend:
Die Funktion `strlen()` ist seit dem Beginn der Sprache C ein Teil der C-Standardbibliothek. Unter der Haube funktioniert sie, indem sie einen Zähler erhöht, während sie den String durchläuft, bis sie auf den Nullterminator trifft. Diese Einfachheit bringt jedoch Leistungsüberlegungen mit sich: Da `strlen()` Zeichen zur Laufzeit zählt, ist es ineffizient, sie wiederholt bei demselben String in einer Schleife aufzurufen, beispielsweise.

In Bezug auf die Sicherheit prüfen `strlen()` und andere C-Stringbehandlungsfunktionen nicht auf Pufferüberläufe, was sorgfältiges Programmieren unerlässlich macht, um Sicherheitsanfälligkeiten zu vermeiden. Moderne Alternativen in anderen Sprachen, wie String-Typen, die die Länge einschließen oder standardmäßig eine sichere Pufferverarbeitung verwenden, beseitigen einige dieser Risiken und Ineffizienzen.

Trotz seiner Einschränkungen ist das Verständnis von `strlen()` und der manuellen String-Handhabung in C für Programmierer von entscheidender Bedeutung, insbesondere bei der Arbeit mit Low-Level-Code oder wenn Leistung und Speicherkontrolle von größter Bedeutung sind. Es bietet auch wertvolle Einblicke in die Funktionsweise von höheren String-Abstraktionen in anderen Sprachen.
