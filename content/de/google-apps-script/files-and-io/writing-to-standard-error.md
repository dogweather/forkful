---
title:                "Ausgabe auf Standardfehler"
aliases:
- /de/google-apps-script/writing-to-standard-error/
date:                  2024-02-01T22:08:48.538700-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ausgabe auf Standardfehler"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/google-apps-script/writing-to-standard-error.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben auf Standardfehler (stderr) in Programmiersprachen handelt davon, Fehlermeldungen und Diagnosen auf einen separaten Datenstrom, abseits der Standardausgabe (stdout), zu leiten. Programmierer tun dies, um normale Programmausgaben von Fehlermeldungen zu trennen, was das Debugging und die Log-Analyse einfacher macht.

## Wie:

Google Apps Script, als eine Skriptsprache für die leichtgewichtige Anwendungsentwicklung auf der Google Apps-Plattform, bietet keine direkte integrierte Funktion wie `console.error()` zum Schreiben auf stderr, wie man sie in Node.js oder Python finden könnte. Sie können dieses Verhalten jedoch simulieren, indem Sie die Protokollierungsdienste von Google Apps Script oder eine benutzerdefinierte Fehlerbehandlung verwenden, um Fehlerausgaben zu verwalten und zu separieren.

### Beispiel: Verwendung von `Logger` für Fehlermeldungen

```javascript
function logError() {
  try {
    // Simuliere einen Fehler
    const result = 1 / 0;
    if(!isFinite(result)) throw new Error("Versuch der Division durch null");
  } catch (e) {
    // Fehlermeldung in Logs schreiben
    Logger.log('Fehler: ' + e.message);
  }
}
```

Wenn Sie `logError()` ausführen, wird die Fehlermeldung in das Log von Google Apps Script geschrieben, das Sie über `Ansicht > Protokolle` einsehen können. Dies ist zwar nicht genau stderr, erfüllt aber einen ähnlichen Zweck, Fehlerprotokolle von Standardausgaben zu trennen.

### Erweiterte diagnostische Protokollierung

Für erweitertes Debugging und Fehlerprotokollierung können Sie Stackdriver Logging, jetzt bekannt als Google Cloud's Operations Suite, verwenden.

```javascript
function advancedErrorLogging() {
  try {
    // Bewusst einen Fehler verursachen
    const obj = null;
    const result = obj.someProperty;
  } catch (e) {
    console.error('Fehler aufgetreten: ', e.toString());
  }
}
```

Dies wird die Fehlermeldung an Stackdriver Logging weiterleiten, wo sie als Fehler-Level-Log verwaltet wird. Beachten Sie, dass die Integration von Stackdriver/Google Cloud’s Operations Suite eine detailliertere und durchsuchbare Lösung für das Logging im Vergleich zu `Logger` bietet.

## Tiefergehende Betrachtung

Das Fehlen eines dedizierten `stderr`-Datenstroms in Google Apps Script spiegelt seine Natur und Ursprünge als cloud-basierte Skriptsprache wider, in der traditionelle Konsolen- oder Terminal-basierte Ausgaben (wie stdout und stderr) weniger relevant sind. Historisch betrachtet, wurde Google Apps Script entworfen, um die Funktionalität von Google Apps mit einfachen Skripten zu erweitern und legte den Fokus auf Benutzerfreundlichkeit anstatt auf umfassende Funktionen, die in komplexeren Programmierumgebungen verfügbar sind.

Dennoch hat die Entwicklung von Google Apps Script hin zu anspruchsvolleren Anwendungsentwicklungen Entwickler dazu veranlasst, kreative Ansätze für Fehlerbehandlung und Protokollierung zu adoptieren, einschließlich der Nutzung verfügbarer Dienste wie Logger und der Integration mit der Google Cloud’s Operations Suite. Diese Methoden bieten, obwohl sie keine direkten Implementierungen von stderr sind, robuste Alternativen für das Fehlermanagement und die diagnostische Protokollierung in einer cloud-zentrischen Umgebung.

Kritischerweise, obwohl diese Methoden innerhalb des Ökosystems von Google Apps Script ihren Zweck erfüllen, unterstreichen sie die Einschränkungen der Plattform im Vergleich zu traditionellen Programmierumgebungen. Für Entwickler, die detaillierte und hierarchische Strategien für die Fehlerbehandlung benötigen, könnten die Integration mit externen Protokollierungsdiensten oder die Nutzung von Google Cloud-Funktionen, die eine konventionellere Behandlung von stderr und stdout bieten, vorzugswürdig sein.
