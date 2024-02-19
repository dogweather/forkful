---
aliases:
- /de/c/reading-command-line-arguments/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:05:58.013267-07:00
description: "In der C-Programmierung erm\xF6glicht das Lesen von Befehlszeilenargumenten,\
  \ dass Programme Eingaben direkt aus dem Terminal akzeptieren k\xF6nnen, was\u2026"
lastmod: 2024-02-18 23:09:05.391724
model: gpt-4-0125-preview
summary: "In der C-Programmierung erm\xF6glicht das Lesen von Befehlszeilenargumenten,\
  \ dass Programme Eingaben direkt aus dem Terminal akzeptieren k\xF6nnen, was\u2026"
title: Kommandozeilenargumente lesen
---

{{< edit_this_page >}}

## Was & Warum?

In der C-Programmierung ermöglicht das Lesen von Befehlszeilenargumenten, dass Programme Eingaben direkt aus dem Terminal akzeptieren können, was Flexibilität und Benutzerfreundlichkeit erhöht. Programmierer nutzen dies, um Skriptverhalten zu konfigurieren, ohne den Code zu ändern, was Anwendungen anpassbar und effizient macht.

## Wie geht das:

In C kann die `main`-Funktion so gestaltet werden, dass sie Befehlszeilenargumente mit den Parametern `int argc` und `char *argv[]` akzeptiert. Hierbei stellt `argc` die Anzahl der übergebenen Argumente dar, und `argv` ist ein Array von Zeichenzeiger, die alle Argumente auflisten. Hier ist ein schnelles Beispiel zur Veranschaulichung:

```c
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Programmname: %s\n", argv[0]);
    printf("Anzahl der Argumente: %d\n", argc - 1);
    for (int i = 1; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

Wird das Programm zum Beispiel als `./programName -a example` ausgeführt, wäre die Ausgabe:

```
Programmname: ./programName
Anzahl der Argumente: 2
Argument 1: -a
Argument 2: example
```

Dies demonstriert, wie Befehlszeilenargumente in einem C-Programm geparst und genutzt werden können.

## Tiefergehende Einblicke

Die Konvention, Argumente an Programme zu übergeben, reicht zurück bis zu den frühesten Tagen von Unix. In diesem traditionellen Ansatz bieten `argc` und `argv` eine einfache, aber leistungsstarke Schnittstelle für die Interaktion mit der Befehlszeile und verkörpern die Philosophie von Unix von kleinen, modularen Hilfsprogrammen, die zusammenarbeiten. Während moderne Sprachen oft anspruchsvollere Bibliotheken oder Frameworks für das Parsen von Befehlszeilenargumenten einführen, bietet die Direktheit der C-Methode unübertroffene Transparenz und Kontrolle.

In jüngeren Entwicklungen haben Bibliotheken wie `getopt` in POSIX-Systemen sich entwickelt, um komplexere Parsing-Bedürfnisse zu unterstützen, wie die Handhabung von langen Optionsnamen oder Standardwerte für fehlende Argumente. Doch bleibt der grundlegende Mechanismus von `argc` und `argv` wesentlich, um zu verstehen, wie Programme in C mit ihrer Laufzeitumgebung interagieren.

Kritiker könnten argumentieren, dass der direkte Umgang mit `argc` und `argv` fehleranfällig sein kann und plädieren für die Nutzung von höheren Abstraktionen. Dennoch, für diejenigen, die bestrebt sind, die Feinheiten von C zu meistern und die Nuancen seines Low-Level-Betriebs zu schätzen, ist das Beherrschen des Parsens von Befehlszeilenargumenten ein Initiationsritus. Diese Mischung aus historischer Methodik und praktischem Nutzen umfasst viel von Cs anhaltender Anziehungskraft in der Systemprogrammierung und Softwareentwicklung.
