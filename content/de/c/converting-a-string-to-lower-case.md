---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Umwandeln einer Zeichemkette in Kleinbuchstaben ist ein alltäglicher Prozess in der Programmierung, bei dem alle Großbuchstaben in einer Zeichenkette (auch bekannt als „string“) in Kleinbuchstaben konvertiert werden. Dies wird häufig gemacht, um die Texteingabe zu vereinheitlichen, Fälle von Groß- und Kleinschreibung zu vermeiden und somit die Datenverarbeitung zu erleichtern.

## Anleitung:

In C können wir die Standard-C-Bibliotheksfunktion `tolower()` verwenden, um einen String zu unteren Fällen zu konvertieren. Hier ist ein einfaches Codebeispiel:

```C
#include <stdio.h>
#include <ctype.h>
#include <string.h>

void convertToLowerCase(char *str)
{
    for(int i = 0; str[i]; i++){
      str[i] = tolower(str[i]);
    }
}

int main() {
    char str[] = "Hallo World!";
    convertToLowerCase(str);

    printf("%s", str);  // Gibt "hallo world!" aus
    return 0;
}
```

## Tief Tauchen:

Historisch betrachtet ist die Funktion `tolower()` Teil der Standard-C-Bibliothek und wird genutzt, um ein einzelnes Zeichen in einen Kleinbuchstaben umzuwandeln. Wenn wir dieses Verfahren auf jeden Buchstaben in einem string anwenden, erhalten wir einen vollständig lowercase string.

Eine alternative Methode, das gleiche Ergebnis zu erreichen, ist der ASCII-Wert jedes Charakters manuell zu überprüfen und zu ändern. Dies kann jedoch zu zusätzlicher Komplexität führen und ist nicht unbedingt empfehlenswert. 

Es ist wichtig zu beachten, dass die Funktion `tolower()` nur für einzelne Zeichen und nicht direkt für ganze Zeichenketten arbeitet. Deshalb müssen wir eine Schleife verwenden, um durch jeden Buchstaben in der Zeichenkette zu gehen, und die `tolower()` Funktion darauf anwenden.

## Siehe auch:

1. [C Library function - tolower()](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
2. [C String to Lower Case](https://stackoverflow.com/questions/26614339/c-string-to-lower-case)

Bitte beachten Sie sowohl die Ressourcen, als auch deren Kontext, um ein besseres Verständnis und methodisches Erfassen der Konversion von Zeichenketten zu Untercases in C zu erreichen.