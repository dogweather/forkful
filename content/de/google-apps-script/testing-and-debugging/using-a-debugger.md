---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:02.788580-07:00
description: "Das Debuggen in Google Apps Script (GAS) umfasst den Prozess der Identifikation\
  \ und Entfernung von Fehlern in Skripten, die dazu bestimmt sind, Google\u2026"
lastmod: '2024-03-11T00:14:27.298700-06:00'
model: gpt-4-0125-preview
summary: "Das Debuggen in Google Apps Script (GAS) umfasst den Prozess der Identifikation\
  \ und Entfernung von Fehlern in Skripten, die dazu bestimmt sind, Google\u2026"
title: Einen Debugger verwenden
---

{{< edit_this_page >}}

## Was & Warum?

Das Debuggen in Google Apps Script (GAS) umfasst den Prozess der Identifikation und Entfernung von Fehlern in Skripten, die dazu bestimmt sind, Google Apps zu automatisieren oder Webanwendungen zu erstellen. Programmierer debuggen, um sicherzustellen, dass ihr Code wie erwartet ausgeführt wird, was Zuverlässigkeit und Leistungsfähigkeit in Anwendungen verbessert.

## Wie:

Google Apps Script stellt einen integrierten Debugger innerhalb des Apps Script-Editors bereit, um beim Troubleshooting von Skripten zu helfen. Hier ist, wie Sie den Debugger initiieren und verwenden:

1. **Öffnen Sie Ihr Skript im Apps Script-Editor.**
2. **Wählen Sie eine Funktion zum Debuggen aus.** Wählen Sie aus dem Dropdown-Menü oben die Funktion aus, die Sie debuggen möchten.
3. **Setzen Sie Haltepunkte.** Klicken Sie auf den Rand (den grauen Bereich links von den Zeilennummern), an dem Sie die Ausführung pausieren möchten; ein roter Punkt erscheint und zeigt einen Haltepunkt an.
4. **Starten Sie das Debugging.** Klicken Sie auf das Käfer-Symbol oder wählen Sie `Debug` > `Start debugging`. Die Ausführung wird starten und am ersten Haltepunkt pausieren.

Betrachten Sie dieses einfache Skript:

```javascript
function calculateSum() {
  var a = 5;
  var b = 10;
  var sum = a + b;
  Logger.log(sum); // Beabsichtigt, 15 zu loggen
}
```

Wenn Sie unsicher sind, warum `Logger.log(sum)` nicht das erwartete Ergebnis anzeigt, könnten Sie einen Haltepunkt bei der Zeile `var sum = a + b;` setzen und das Skript Zeile für Zeile durchgehen, um die Werte der Variablen zu inspizieren.

**Beispielausgabe im Logger:**

```plain
15
```

Während des Debuggens ermöglicht Ihnen der Apps Script-Editor:

- **Durch den Code zu schreiten** mithilfe der Schaltflächen zum Überspringen, Eintreten und Austritt.
- **Ausdrücke und Variablen zu beobachten**, um ihre Werte in Echtzeit zu sehen.
- **Den Aufrufstapel zu inspizieren**, um Funktionsaufrufe nachzuverfolgen.

## Vertiefung

Wie in jeder anderen Programmierumgebung ist das Debuggen in Google Apps Script unerlässlich für die Erstellung fehlerfreier Anwendungen. Der integrierte Debugger, der früh in der Entwicklung von GAS eingeführt wurde, bietet grundlegende Fähigkeiten, um Code schrittweise zu inspizieren und zu korrigieren. Obwohl er grundlegende Debugging-Funktionen bietet, die jenen in ausgereifteren Umgebungen wie Visual Studio Code oder IntelliJ ähnlich sind, könnte er für komplexe Debugging-Szenarien zu kurz greifen. Zum Beispiel könnten seine Fähigkeiten, asynchrone Callbacks zu inspizieren oder schwere Skriptausführungen zu verwalten, begrenzt sein.

Für komplexe Debugging-Bedürfnisse könnten Entwickler auf alternative Methoden wie umfangreiches Logging (mit `Logger.log()`) oder sogar das Deployment als Web-App zurückgreifen, um das Verhalten in einem realen Szenario zu inspizieren. Dennoch macht die Einfachheit und Integration des GAS-Debuggers im Apps Script-Editor ihn zu einem unschätzbaren ersten Schritt für die Fehlerbehebung und das Verständnis des Skriptverhaltens. Insbesondere mit den kontinuierlichen Updates und Verbesserungen von Google an Apps Script verbessert sich die Debugging-Erfahrung stetig und bietet im Laufe der Zeit ausgefeiltere Tools und Optionen. Diese Entwicklung spiegelt Googles Engagement wider, Apps Script zu einer leistungsstärkeren und zugänglicheren Plattform für Entwickler aus unterschiedlichsten Bereichen zu machen.
