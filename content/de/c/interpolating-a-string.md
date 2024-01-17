---
title:                "Eine Zeichenkette interpolieren"
html_title:           "C: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Beim sogenannten String Interpolieren geht es darum, Platzhalter in einem String mit konkreten Werten zu ersetzen. Zum Beispiel könnte ein String "Hallo {Name}" lauten und durch die Interpolation könnte der Platzhalter "{Name}" mit dem Wert "Peter" zu "Hallo Peter" werden. Programmierer nutzen diese Technik, um dynamische Texte zu erstellen und Wiederholungen zu vermeiden.

## Wie geht's?

Um einen String zu interpolieren, gibt es verschiedene Möglichkeiten. Eine davon ist die Verwendung von der Funktion "sprintf()". Dabei wird dem String, der interpoliert werden soll, ein Formatierungsschema übergeben, das die Platzhalter beinhaltet. Nach dem Schema wird der Funktion dann eine Liste von Werten übergeben, die die Platzhalter in der gleichen Reihenfolge wie im Schema ersetzen.

Ein Beispiel:

```C
char greeting[20];
char name[10] = "Peter";

sprintf(greeting, "Hallo %s", name);

printf("%s", greeting); // Ausgabe: "Hallo Peter"
```

## Tiefere Einblicke

String Interpolation ist keine neue Technik und wurde bereits in früheren Versionen von C verwendet. Allerdings gibt es auch alternative Methoden, um Platzhalter in Strings zu ersetzen, wie z.B. die Verwendung von regulären Ausdrücken.

In der aktuellen Version von C wird String Interpolation nicht explizit unterstützt, aber durch die Verwendung von Funktionen wie "sprintf()" oder auch "snprintf()" kann diese Technik trotzdem umgesetzt werden. Es ist jedoch wichtig zu beachten, dass die Werte, die zur Interpolation verwendet werden, korrekt formatiert sein müssen.

## Weitere Informationen

Für eine ausführlichere Beschreibung von String Interpolation in C und alternative Methoden können folgende Links hilfreich sein:

- Link 1
- Link 2
- Link 3