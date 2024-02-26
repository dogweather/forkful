---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:40.975270-07:00
description: "Debugger in C sind spezialisierte Werkzeuge, die es Entwicklern erm\xF6\
  glichen, Schritt f\xFCr Schritt durch ihren Code zu gehen, Variablen zu \xFCberpr\xFC\
  fen und\u2026"
lastmod: '2024-02-25T18:49:51.407593-07:00'
model: gpt-4-0125-preview
summary: "Debugger in C sind spezialisierte Werkzeuge, die es Entwicklern erm\xF6\
  glichen, Schritt f\xFCr Schritt durch ihren Code zu gehen, Variablen zu \xFCberpr\xFC\
  fen und\u2026"
title: Verwenden eines Debuggers
---

{{< edit_this_page >}}

## Was & Warum?

Debugger in C sind spezialisierte Werkzeuge, die es Entwicklern ermöglichen, Schritt für Schritt durch ihren Code zu gehen, Variablen zu überprüfen und den Ausführungsfluss zu überwachen. Dieser Prozess ist integraler Bestandteil der Identifizierung und Behebung von Fehlern, um sicherzustellen, dass der Code wie erwartet funktioniert.

## Wie geht das:

GDB (GNU Debugger) ist der am häufigsten verwendete Debugger für die C-Programmierung. Hier ist eine kurze Anleitung zur Verwendung von GDB zum Debuggen eines einfachen C-Programms.

Zuerst kompilieren Sie Ihr C-Programm mit dem `-g` Flag, um Debugging-Informationen einzuschließen:

```c
gcc -g program.c -o program
```

Starten Sie als Nächstes GDB mit Ihrem kompilierten Programm:

```bash
gdb ./program
```

Sie können jetzt verschiedene Befehle innerhalb von GDB verwenden, um dessen Betrieb zu steuern. Hier sind einige grundlegende Befehle:

- `break`: Setzt einen Haltepunkt an einer angegebenen Zeile oder Funktion, um die Ausführung anzuhalten.
  - Beispiel: `break 10` oder `break main`
- `run`: Startet die Ausführung Ihres Programms innerhalb von GDB.
- `next`: Führt die nächste Zeile des Codes aus, ohne in Funktionen zu gehen.
- `step`: Führt die nächste Zeile des Codes aus und tritt dabei in Funktionen ein.
- `print`: Zeigt den Wert einer Variablen an.
- `continue`: Setzt die Ausführung fort, bis zum nächsten Haltepunkt.
- `quit`: Beendet GDB.

Hier ist ein Beispiel für eine Debugging-Sitzung mit einem einfachen Programm:

```c
#include <stdio.h>

int main() {
    int i;
    for (i = 0; i < 5; i++) {
        printf("%d\n", i);
    }
    return 0;
}
```

Kompilieren Sie das Programm und starten Sie GDB wie beschrieben. Setzen Sie einen Haltepunkt bei der `printf` Zeile mit `break 5` und dann `run`. Verwenden Sie `next`, um durch die Schleife zu gehen, und `print i`, um die Schleifenvariable zu überprüfen.

Beispielausgabe nach dem Setzen eines Haltepunkts und vor der ersten Iteration:

```
Breakpoint 1, main () at program.c:5
5         printf("%d\n", i);
```

Verwendung von `print i` nach einigen Iterationen:

```
$3 = 2
```

Dies demonstriert die Untersuchung des Zustands und des Ablaufs eines einfachen Programms.

## Vertiefung

Das Konzept des Debuggings hat sich seit den frühen Tagen der Programmierung erheblich weiterentwickelt, als physische Fehler (buchstäbliche Insekten) Probleme in mechanischen Computern verursachen konnten. Heute bieten Debugger wie GDB ausgefeilt Funktionen über das grundlegende Schritt-für-Schritt-Durchgehen und Variableninspektion hinaus, wie das Rückwärtsdebugging (Ausführen des Programms rückwärts), bedingte Haltepunkte und Skripting für automatisierte Debugging-Aufgaben.

Obwohl GDB leistungsfähig und weit verbreitet ist, kann es für Anfänger dicht und herausfordernd sein. Alternative Debugging-Werkzeuge und IDEs (integrierte Entwicklungsumgebungen) wie Visual Studio Code, CLion oder Eclipse bieten benutzerfreundlichere Schnittstellen für das Debuggen von C-Code, oft mit visuellen Hilfen und intuitiveren Steuerelementen. Diese Alternativen bieten möglicherweise nicht die volle Funktionalität von GDB, können aber für Neulinge in der C-Programmierung zugänglicher sein.

Darüber hinaus hat das Aufkommen von Language Server Protokollen und Debugging-Standards plattformübergreifende Debugging-Lösungen erleichtert, was das Debugging-Erlebnis über verschiedene Werkzeuge und Umgebungen hinweg konsistenter macht. Trotz dieser Fortschritte bietet das Erlernen der Feinheiten eines traditionellen Debuggers wie GDB wertvolle Einblicke in die Ausführung von C-Programmen und bleibt eine entscheidende Fähigkeit im Werkzeugkasten eines Entwicklers.
