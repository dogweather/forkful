---
title:                "Einen String großschreiben"
html_title:           "C: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Strings zu Grossbuchstaben umwandeln, bedeutet, jeden Buchstaben innerhalb eines Strings in seinen entsprechenden Grossbuchstaben umzuwandeln. Programmierer machen das hauptsächlich, um die Texteingabe zu standardisieren und unabhängig von der Eingabeform des Nutzers zu machen.

## Anleitung:

Das folgende ist ein einfaches C-Programm, das einen String zu Grossbuchstaben umwandelt:

```C
#include<stdio.h>
#include<ctype.h>
#include<string.h>

void stringToupper(char *s) {
   for(int i = 0; i < strlen(s); i++){
      s[i] = toupper(s[i]);
   }
}

int main() {
   char s[] = "Hallo Welt!";
   stringToupper(s);
   printf("%s", s);
   return 0;
}
```
Verwenden Sie den `toupper` Funktion, um jeden Buchstaben in einen Grossbuchstaben umzuwandeln. Wenn dieses Programm ausgeführt wird, druckt es `HALLO WELT!`.

## Vertiefung:

Die `toupper` Funktion ist teil der `ctype.h` Bibliothek und sie wurde in der frühen Version von C eingeführt. Im Kontext des Umgangs mit Strings ist sie sehr praktisch.

Alternativ könnten wir die ASCII-Codierung verwenden, um Grossbuchstaben zu erzeugen, but `toupper` nimmt uns diese manuelle Arbeit ab. Ein weiterer Vorteil von `toupper` ist, dass sie funktioniert, auch wenn der Eingabe-String bereits Grossbuchstaben enthält.

Die Implementierung von `toupper` kann von System zu System variieren, aber im Allgemeinen überprüft sie, ob der übergebene Charakter ein Kleinbuchstabe ist, und kehrt zurück, falls dies nicht der Fall ist. Wenn der Charakter ein Kleinbuchstabe ist, wird er durch Abzug des Unterschieds zwischen ASCII-Werten von 'a' und 'A' in einen Grossbuchstaben umgewandelt.

## Siehe auch:

Weitere Informationen zu den Themen in diesem Artikel finden Sie unter den folgenden Links:
- [toupper function](https://en.cppreference.com/w/c/string/byte/toupper)
- [ctype.h library](https://en.cppreference.com/w/c/string/byte)
- [ASCII Table and Description](http://www.asciitable.com/)