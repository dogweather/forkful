---
title:                "Einsatz eines Debuggers"
date:                  2024-01-26T04:10:51.168303-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einsatz eines Debuggers"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/using-a-debugger.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein Debugger ist ein Werkzeug, das es Ihnen ermöglicht, die inneren Abläufe Ihres Codes während der Ausführung zu untersuchen und zu ändern. Programmierer nutzen es, um Fehler zu beheben, indem sie durch ihren Code schrittweise gehen, Variablen inspizieren und den Fluss ihres Programms verstehen.

## Wie geht das:

Um mit einem Debugger in TypeScript durchzustarten, benötigen Sie lediglich eine unterstützte IDE (wie Visual Studio Code) und eine `launch.json`-Konfiguration. Hier ist ein schnelles Beispiel für eine Node.js-Anwendung:

```TypeScript
// app.ts
function greet(name: string) {
    console.log(`Hallo, ${name}!`);
}

const userName = 'Ada';
greet(userName);
```

Um dies zu debuggen, erstellen Sie eine `launch.json`-Datei im Ordner `.vscode`:

```JSON
{
    "version": "0.2.0",
    "Konfigurationen": [
        {
            "Typ": "node",
            "Anforderung": "launch",
            "Name": "Programm starten",
            "ÜberspringeDateien": ["<node_internals>/**"],
            "Programm": "${workspaceFolder}/app.ts",
            "VorStartAufgabe": "tsc: build - tsconfig.json",
            "Ausgabedateien": ["${workspaceFolder}/build/**/*.js"]
        }
    ]
}
```

Setzen Sie dann einen Haltepunkt in Ihrer `greet`-Funktion, indem Sie auf der linken Seite der Zeilennummer in Ihrer IDE klicken. Drücken Sie F5, um das Debugging zu starten, und beobachten Sie, wie Ihre App am Haltepunkt pausiert. Jetzt können Sie Variablen überfliegen, Ausdrücke überwachen und mit Leichtigkeit durch Ihren Code schreiten.

## Tiefer eintauchen

In der Zeit, bevor integrierte Entwicklungsumgebungen (IDEs) ausgefeilt wurden, erfolgte das Debuggen oft mit Druckanweisungen (auch bekannt als `console.log`-Debugging). Es funktionierte, irgendwie, war aber wie die Suche nach einer Nadel im Heuhaufen mit verbundenen Augen.

Moderne Debugger sind wie ein Schweizer Taschenmesser für die Fehlersuche. Mit der Entwicklung von TypeScript und Node.js gibt es verschiedene Debugger, vom eingebauten Node.js-Inspektor bis hin zu Browser-Entwicklungstools für das Client-seitige Debugging.

Der Node.js-Inspektor funktioniert, indem er sich an Ihre laufende Anwendung anheftet; er kommuniziert über das Chrome DevTools-Protokoll und verwandelt Ihren Chrome-Browser in eine mächtige Debugging-Konsole. Diese Integration ermöglicht eine visuell interaktive und detaillierte Debugging-Sitzung im Vergleich zu traditionellen Debugging-Praktiken über die Befehlszeile.

## Siehe auch

Für ein wenig zusätzliche Lektüre und einige Profi-Tipps, schauen Sie sich an:

- [TypeScript-Debugging in Visual Studio Code](https://code.visualstudio.com/docs/typescript/typescript-debugging)
- [Node.js-Debugging-Anleitung](https://nodejs.org/en/docs/guides/debugging-getting-started/)
- [Chrome DevTools-Dokumentation](https://developers.google.com/web/tools/chrome-devtools)
