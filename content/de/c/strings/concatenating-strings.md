---
title:                "Strings verketten"
aliases:
- /de/c/concatenating-strings/
date:                  2024-02-03T17:53:50.789132-07:00
model:                 gpt-4-0125-preview
simple_title:         "Strings verketten"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/concatenating-strings.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Verketten von Strings in C bezieht sich auf das Zusammenführen von zwei oder mehr Strings hintereinander, um einen neuen String zu bilden. Programmierer führen diese Operation durch, um Strings zur Laufzeit dynamisch zu erstellen, was essentiell ist für das Erstellen von bedeutungsvollen Nachrichten, Dateipfaden oder jeglichen Daten, die aus verschiedenen Stringquellen zusammengesetzt sind.

## Wie:

In C sind Strings Arrays von Zeichen, die mit einem Nullzeichen (`\0`) enden. Im Gegensatz zu höheren Programmiersprachen bietet C keine integrierte Stringverkettungsfunktion. Stattdessen verwendet man die Funktionen `strcat()` oder `strncat()` aus der Bibliothek `<string.h>`.

Hier ist ein einfaches Beispiel mit `strcat()`:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char ziel[50] = "Hallo, ";
    char quelle[] = "Welt!";

    strcat(ziel, quelle);

    printf("%s\n", ziel);  // Ausgabe: Hallo, Welt!
    return 0;
}
```

Die Funktion `strcat()` nimmt zwei Argumente: den Zielstring (der genug Platz haben muss, um das Ergebnis der Verkettung zu speichern) und den Quellstring. Anschließend wird der Quellstring an den Zielstring angehängt.

Für mehr Kontrolle über die Anzahl der verketteten Zeichen ist `strncat()` sicherer zu verwenden:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char ziel[50] = "Hallo, ";
    char quelle[] = "Welt!";
    int num = 3; // Anzahl der anzuhängenden Zeichen

    strncat(ziel, quelle, num);

    printf("%s\n", ziel);  // Ausgabe: Hallo, Wer
    return 0;
}
```

Dies begrenzt die Verkettung auf die ersten `num` Zeichen des Quellstrings und hilft, Buffer-Überläufe zu verhindern.

## Vertiefung

Die Funktionen `strcat()` und `strncat()` sind seit dem Beginn der C-Standardbibliothek ein Teil davon, was die tiefgreifende, niedrigstufige Natur der Sprache widerspiegelt, die eine manuelle Verwaltung von Strings und Speicher erfordert. Im Gegensatz zu vielen modernen Programmiersprachen, die Strings als Objekte erster Klasse mit eingebauten Verkettungsoperatoren behandeln (wie `+` oder `.concat()`), erfordert C's Ansatz ein tieferes Verständnis von Zeigern, Speicherzuweisung und potenziellen Fallstricken wie Buffer-Überläufen.

Obwohl `strcat()` und `strncat()` weit verbreitet sind, werden sie oft für ihr Potenzial kritisiert, Sicherheitsanfälligkeiten zu schaffen, wenn sie nicht sorgfältig verwendet werden. Buffer-Überläufe, bei denen Daten den zugewiesenen Speicher überschreiten, können zu Abstürzen führen oder für die Ausführung von beliebigem Code ausgenutzt werden. Daher wenden sich Programmierer zunehmend sichereren Alternativen zu, wie `snprintf()`, welche ein vorhersehbares Verhalten bieten, indem sie die Anzahl der Zeichen, die in den Zielstring geschrieben werden, basierend auf dessen Größe begrenzen:

```c
char ziel[50] = "Hallo, ";
char quelle[] = "Welt!";
snprintf(ziel + strlen(ziel), sizeof(ziel) - strlen(ziel), "%s", quelle);
```

Diese Methode ist umständlicher, aber deutlich sicherer und unterstreicht einen Wandel in den C-Programmierpraktiken hin zur Priorisierung von Sicherheit und Robustheit über Kürze.

Trotz dieser Herausforderungen ist das Verketten von Strings in C eine grundlegende Fähigkeit, die für effektives Programmieren in der Sprache entscheidend ist. Das Verständnis seiner Feinheiten und der damit verbundenen Risiken ist der Schlüssel zur Meisterung der C-Programmierung.
