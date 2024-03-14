---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:27.567013-07:00
description: "Tests in Google Apps Script (GAS) zu schreiben, bedeutet, automatisierte\
  \ Skripte zu erstellen, um das Verhalten Ihrer Codes zu \xFCberpr\xFCfen und\u2026"
lastmod: '2024-03-13T22:44:53.337885-06:00'
model: gpt-4-0125-preview
summary: "Tests in Google Apps Script (GAS) zu schreiben, bedeutet, automatisierte\
  \ Skripte zu erstellen, um das Verhalten Ihrer Codes zu \xFCberpr\xFCfen und\u2026"
title: Tests schreiben
---

{{< edit_this_page >}}

## Was & Warum?

Tests in Google Apps Script (GAS) zu schreiben, bedeutet, automatisierte Skripte zu erstellen, um das Verhalten Ihrer Codes zu überprüfen und sicherzustellen, dass sie unter verschiedenen Bedingungen wie erwartet funktionieren. Programmierer tun dies, um frühzeitig Fehler zu erkennen, die Codequalität zu verbessern und Updates sowie Wartungsarbeiten zu erleichtern.

## Wie geht das:

Obwohl Google Apps Script kein eingebautes Test-Framework wie einige andere Programmierumgebungen hat, können Sie immer noch Tests schreiben und ausführen, indem Sie einfache GAS-Funktionen nutzen oder externe Testbibliotheken wie `QUnit` integrieren. Hier ist ein grundlegendes Beispiel, das eine einfache GAS-Funktion verwendet, um eine andere Funktion in Ihrem Skript zu testen:

```javascript
function add(a, b) {
  return a + b;
}

function testAdd() {
  var result = add(2, 3);
  if (result !== 5) {
    throw new Error("Test fehlgeschlagen: add(2, 3) sollte 5 sein, war aber " + result);
  } else {
    Logger.log("Test bestanden!");
  }
}
```

Wenn Sie `testAdd()` ausführen, wird "Test bestanden!" protokolliert, wenn die `add`-Funktion korrekt funktioniert, oder ein Fehler ausgelöst, wenn dies nicht der Fall ist. Für einen anspruchsvolleren Ansatz erfordert die Integration von QUnit mit Google Apps Script einige zusätzliche Schritte, bietet aber eine leistungsfähige Testumgebung. Eine beispielhafte QUnit-Testeinrichtung sieht folgendermaßen aus:

1. Fügen Sie die QUnit-Bibliothek zu Ihrem Projekt hinzu.
2. Erstellen Sie eine Test-HTML-Datei zum Ausführen der QUnit-Tests.
3. Schreiben Sie Testfälle unter Verwendung der QUnit-Syntax.

Hier ist ein Beispiel mit QUnit:

```javascript
// QUnit einbinden, indem Sie es in einer HTML-Datei verlinken, die verwendet wird, um Ihre Tests auszuführen

QUnit.test("Test der Add-Funktion", function (assert) {
  var result = add(2, 3);
  assert.equal(result, 5, "add(2, 3) sollte 5 zurückgeben");
});
```

Um die Ergebnisse zu sehen, öffnen Sie die HTML-Datei im GAS-Skripteditor oder stellen Sie sie als Web-App bereit.

## Tiefergehend

Historisch gesehen wurde das Testen in Google Apps Script etwas vernachlässigt, was wahrscheinlich auf die Ursprünge der Plattform und die primären Anwendungsfälle zurückzuführen ist, die sich auf schnelle, kleinere Automatisierungsaufgaben konzentrierten, statt auf große Anwendungen. Daher bietet GAS nicht die gleichen robusten Test-Frameworks und -Tools, die in traditionelleren Programmierumgebungen zu finden sind. Die Community hat sich jedoch angepasst, indem sie Open-Source-Bibliotheken einbezogen und vorhandene Tools von Google kreativ genutzt hat.

Die Verwendung von Bibliotheken wie QUnit stellt einen bedeutenden Schritt nach vorne dar, bringt jedoch eigene Herausforderungen mit sich, wie das Einrichten einer geeigneten Testumgebung und das Erlernen einer zusätzlichen Syntax. Für diejenigen, die jedoch in den Aufbau komplexerer und zuverlässigerer Anwendungen mit GAS investiert sind, lohnt sich der Aufwand.

Alternativen wie die Verwendung einfacher GAS-Funktionen für Tests bieten Benutzerfreundlichkeit und Integration in die GAS-Umgebung ohne zusätzliche Abhängigkeiten, aber es fehlen umfassende Testfunktionen und die Fähigkeit, leicht mit Ihrem Projekt zu skalieren. Tools wie clasp (die Google Apps Script Command Line Interface) können fortgeschrittenere Arbeitsabläufe, einschließlich Tests, erleichtern, indem sie Entwicklern erlauben, in ihrer bevorzugten IDE zu programmieren und so Raum für die Integration mit externen Testframeworks nahtloser zu schaffen.

Zusammenfassend lässt sich sagen, dass GAS zwar keine native Unterstützung für anspruchsvolle Tests direkt aus der Box bietet, seine Flexibilität und die innovativen Ansätze der Community jedoch praktikable Wege bieten, um sicherzustellen, dass Ihre Skripte robust, zuverlässig und bereit für jede Aufgabe sind.
