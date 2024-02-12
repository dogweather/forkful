---
title:                "Mit einer interaktiven Shell (REPL) arbeiten"
date:                  2024-02-03T18:10:10.058812-07:00
model:                 gpt-4-0125-preview
simple_title:         "Mit einer interaktiven Shell (REPL) arbeiten"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/using-an-interactive-shell-repl.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was und Warum?

Eine interaktive Shell, auch bekannt als Read-Eval-Print Loop (REPL), ermöglicht es Programmierern, Ausdrücke oder Code einzugeben und sofort Ergebnisse zu sehen, was das Lernen und den Debugging-Prozess verbessert. Obwohl C traditionell keine REPL-Umgebungen nativ unterstützt, schließen moderne Werkzeuge diese Lücke und bieten eine dynamische Erkundung von C-Programmen.

## Wie geht das:

Den Einstieg in eine C-REPL zu finden, ist vielleicht nicht so geradlinig wie in Sprachen wie Python oder JavaScript. Jedoch machen Werkzeuge wie `Cling`, ein C/C++ Interpreter, der auf Clang und LLVM-Technologie basiert, es möglich. So kommen Sie los:

1. **Cling installieren**: Abhängig von Ihrem Betriebssystem, finden Sie Cling möglicherweise in Ihrem Paketmanager oder müssen es aus dem Quellcode erstellen. Zum Beispiel ist es auf Ubuntu so einfach wie `sudo apt-get install cling`.

2. **Cling starten**: Öffnen Sie Ihr Terminal und geben Sie `cling` ein, um die interaktive Shell zu starten.

```bash
$ cling
```

3. **Code schreiben**: Jetzt können Sie direkt in die Shell C-Code eingeben und sofortige Ergebnisse sehen. Hier ein einfaches Beispiel:

```c
[cling]$ #include <stdio.h>
[cling]$ printf("Hallo, REPL-Welt!\n");
Hallo, REPL-Welt!
```

4. **Beispiel mit Variablen und Operationen**: Experimentieren Sie mit Variablen und sehen Sie sofortiges Feedback.

```c
[cling]$ int a = 5;
[cling]$ int b = 3;
[cling]$ printf("%d + %d = %d\n", a, b, a+b);
5 + 3 = 8
```

5. **Bibliotheken einbinden**: Cling ermöglicht es Ihnen, Bibliotheken on-the-fly einzubinden, und ermöglicht so eine breite Palette von C-Funktionalitäten.

```c
[cling]$ #include <math.h>
[cling]$ printf("Quadratwurzel von %f ist %f\n", 4.0, sqrt(4.0));
Quadratwurzel von 4.000000 ist 2.000000
```

## Tiefergehendes:

Die Erfindung von REPL-Umgebungen reicht zurück bis zu Lisp in den 1960er Jahren, entworfen zur Unterstützung interaktiver Code-Auswertung. Jedoch stellte die statische und kompilierte Natur von C Herausforderungen dar, eine ähnliche Unmittelbarkeit in der Code-Ausführungsanpassung zu realisieren. Die Entwicklung von Cling und anderen C/C++ Interpretern markiert signifikante Fortschritte zur Integration dynamischer Bewertung in statisch typisierte Sprachen.

Beachtenswert ist, dass die Verwendung eines Interpreters wie Cling das Verhalten von kompiliertem C-Code aufgrund von Unterschieden in der Optimierung und Ausführung nicht perfekt widerspiegeln kann. Auch sind REPLs für C, obwohl sie für Bildungszwecke, schnelles Prototyping und Debugging sehr wertvoll sind, manchmal langsamer und weniger praktisch für die Entwicklung von produktionsreifem Code im Vergleich zu traditionellen Kompilieren-Ausführen-Debugging-Zyklen.

Alternativen für interaktive C-Programmierung umfassen das Schreiben von kleinen, eigenständigen Programmen und die Verwendung von robusten IDEs mit integrierten Debugging-Tools, die mehr Kontrolle und Einblick in die Ausführung bieten können, allerdings mit weniger Unmittelbarkeit. Trotz dieser Alternativen stellt der Aufkommen von REPL-Umgebungen in C eine aufregende Erweiterung der Vielseitigkeit der Sprache dar und umarmt die Forderungen des modernen Zeitalters nach Flexibilität und Geschwindigkeit in den Entwicklungszyklen.
