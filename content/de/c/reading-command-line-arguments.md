---
title:                "C: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Manchmal müssen wir unseren C-Code ausführen und ihm zusätzliche Informationen geben, ohne diese direkt im Code festzulegen. Hier kommen die Befehlszeilenargumente ins Spiel. Sie ermöglichen es uns, Parameter an unser Programm zu übergeben, ohne den Code jedes Mal ändern zu müssen.

## How To

Befehlszeilenargumente können in der `main` Funktion als Argumente übergeben werden. Zum Beispiel:

```C
int main(int argc, char *argv[]) {
    // Code hier
    return 0;
}
```

`argc` ist eine Variable, die die Anzahl der Argumente, die an das Programm übergeben wurden, speichert. `argv[]` ist ein Array, das die eigentlichen Argumente enthält, die als Zeichenfolgen gespeichert sind.

Lassen Sie uns ein Beispiel betrachten. Nehmen wir an, wir haben folgendes Programm:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Hallo %s!", argv[1]);
    return 0;
}
```

Wenn wir dieses Programm mit dem Befehl `./programm Max` ausführen, wird das Programm `Hallo Max!` ausgeben. Hier ist `Max` das erste Argument, das wir dem Programm übergeben haben.

## Deep Dive

Neben einfachen Zeichenfolgen können Befehlszeilenargumente auch verwendet werden, um Zahlen zu übergeben. Wir müssen jedoch sicherstellen, dass wir die Zeichenfolgen in entsprechende Datentypen konvertieren, bevor wir sie verwenden.

Zum Beispiel, wenn wir einen Integer als Befehlszeilenargument übergeben möchten, müssen wir es wie folgt konvertieren:

```C
int myArg = atoi(argv[1]);
```

Dies wäre nützlich, wenn wir bspw. eine Anzahl von wiederholten Durchgängen in einem Schleifenausdruck übergeben möchten.

## Siehe auch

- [Argumente an C-Programme übergeben](https://www.dummies.com/programming/c/how-to-pass-command-line-arguments-to-your-c-program/)
- [Die Anweisung 'main'](https://openbook.rheinwerk-verlag.de/c_von_a_bis_z/014_c_laboratorium_003.htm)
- [Die Funktion 'atoi'](https://www.tutorialspoint.com/c_standard_library/c_function_atoi.htm)