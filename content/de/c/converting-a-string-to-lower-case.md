---
title:                "C: Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Warum
Das Konvertieren einer Zeichenfolge in Kleinbuchstaben mag auf den ersten Blick wie eine einfache Aufgabe erscheinen, aber es kann in bestimmten Situationen sehr nützlich sein. Zum Beispiel könnte es helfen, bei der Eingabe von Benutzernamen oder Passwörtern die Groß- und Kleinschreibung zu ignorieren und so Fehler zu vermeiden.

## Wie man es macht
Der folgende Code zeigt, wie man eine Zeichenfolge in Kleinbuchstaben umwandelt:

```c
#include <stdio.h>
#include <ctype.h>

int main(){
    char str[] = "Hallo WELT!";
    int i = 0;

    // Schleife durchläuft alle Zeichen der Zeichenfolge
    while (str[i]){
        // Jedes Zeichen wird mit der Funktion tolower() in Kleinbuchstaben umgewandelt
        str[i] = tolower(str[i]);
        i++;
    }

    // Ausgabe der umgewandelten Zeichenfolge
    printf("Umgewandelt: %s", str);
    return 0;
}
```

**Ausgabe:** umgewandelt: hallo welt!

Wie man sieht, wird jedes Zeichen in der Schleife mit Hilfe der Funktion `tolower()` in Kleinbuchstaben umgewandelt. Dabei ist zu beachten, dass die Funktion nur für ASCII-Zeichen funktioniert.

## Tieferer Einblick

Um eine Zeichenfolge erfolgreich in Kleinbuchstaben zu konvertieren, ist es wichtig zu verstehen, dass es sich bei Strings in C um Arrays von Zeichen handelt. Deshalb können wir in der Schleife einfach auf jedes Element des Arrays `str` zugreifen und dieses mit `tolower()` umwandeln.

Eine wichtige Sache, die man beachten sollte, ist, dass die Funktion `tolower()` nur dann angewendet werden sollte, wenn sich das Zeichen tatsächlich in Großbuchstaben befindet. Ansonsten kann es zu unerwünschten Ergebnissen führen. Um dies zu überprüfen, kann man die Funktion `isupper()` verwenden, die prüft, ob es sich bei dem Zeichen um einen Großbuchstaben handelt.

Außerdem ist es möglich, eine andere Funktion wie `toupper()` zu verwenden, um eine Zeichenfolge in Großbuchstaben umzuwandeln.

## Siehe auch

- [ASCII-Zeichen - Wikipedia](https://de.wikipedia.org/wiki/American_Standard_Code_for_Information_Interchange)
- [Stringfunktionen in C - GeeksforGeeks](https://www.geeksforgeeks.org/string-functions-in-c/)
- [Strings in C - Tutorialspoint](https://www.tutorialspoint.com/cprogramming/c_strings.htm)