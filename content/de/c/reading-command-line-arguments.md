---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Was & Warum?
In C ermöglicht das Lesen von Befehlszeilenargumenten den Empfang von Eingabewerten direkt bei der Ausführung eines Programms. Dies ist nützlich, um unser Programm dynamischer zu gestalten, indem wir es unterschiedlichen Situationen anpassen, ohne den Code ändern zu müssen.

# Wie es geht:
Für die Definition von argc (Argumentanzahl) und argv (Argumentvektor) sieht die Haupteintragung des C-Programms wie folgt aus:
```C
int main(int argc, char *argv[]) {
// Programmcode
}
```
Zum Beispiel könnten wir eine kurze Anwendung erstellen, die alle übergebenen Argumente ausgibt:
```C
#include<stdio.h>
int main(int argc, char *argv[]) {
    for(int i=0; i<argc; i++) {
        printf("Argument %d ist %s\n", i, argv[i]);
    }
    return 0;
}
```
Wenn Sie dieses Programm mit `./myprogram arg1 arg2 arg3` aufrufen, sieht die Ausgabe folgendermaßen aus:
```
Argument 0 ist ./myprogram
Argument 1 ist arg1
Argument 2 ist arg2
Argument 3 ist arg3
```

# Vertiefung
Historisch gesehen sind Befehlszeilenargumente ein Erbe vieler früherer Systeme, darunter Unix. Sie wurden entwickelt, um einigen der frühen Einschränkungen dieser Systeme entgegenzuwirken und gleichzeitig ein hohes Maß an Flexibilität bei der Kommandozeilenausführung von Programmen zu ermöglichen.

Alternativen zu Befehlszeilenargumenten umfassen das Einlesen von Dateien, Umgebungsvariablen und interaktiven Benutzereingaben. Obwohl diese nützlich sind, bieten sie nicht die gleiche direkte und einfache Mechanik, die Befehlszeilenargumente ermöglichen.

Befehlszeilenargumente werden vom Betriebssystem angelegt und beim Start des Programms auf den Stack gelegt. Aus diesem Grund haben sie einen begrenzten Einsatzbereich und nicht jedes Programm muss argc und argv verwenden.

# Siehe Auch
- Einführung in die C-Programmierung: https://www.learn-c.org/
- Tiefergehende Informationen zum Umgang mit Befehlszeilenargumenten in C: https://www.cprogramming.com/tutorial/c/lesson14.html
- Unterschiede im Umgang mit Befehlszeilenargumenten je nach Betriebssystem: https://www.geekhideout.com/urlcode.shtml