---
title:                "Refactoring"
aliases: - /de/google-apps-script/refactoring.md
date:                  2024-02-01T22:00:33.060270-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/google-apps-script/refactoring.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Refactoring im Programmierlexikon bezieht sich auf den Prozess des Umstrukturierens bestehenden Computer-Codes – das Ändern der Faktorierung ohne Änderung seines externen Verhaltens – um nicht-funktionale Attribute zu verbessern. Es ist ein entscheidender Schritt für Programmierer, um die Lesbarkeit des Codes zu erhöhen, Komplexität zu reduzieren und potenziell verborgene Fehler aufzudecken, was eine einfachere Wartung und zukünftige Skalierbarkeit des Codes fördert.

## Wie:

In Google Apps Script ist ein häufiges Szenario, das vom Refactoring profitiert, die Vereinfachung von umständlichen Skripten, die mit Google Sheets oder Docs interagieren. Anfangs könnten Skripte auf eine schnelle und schmutzige Weise geschrieben werden, um schnell Ergebnisse zu erzielen. Mit der Zeit, wenn das Skript wächst, wird es unhandlich. Lassen Sie uns ein Beispiel für Refactoring für bessere Lesbarkeit und Effizienz durchgehen.

**Ursprüngliches Skript:**

```javascript
function logSheetNames() {
  var sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  for (var i = 0; i < sheets.length; i++) {
    Logger.log(sheets[i].getName());
  }
}
```

Diese Funktion protokolliert den Namen jedes Blatts in einem Google Spreadsheet. Obwohl es gut funktioniert, verwendet es veraltete JavaScript-Praktiken und mangelt an Klarheit.

**Überarbeitetes Skript:**

```javascript
function logSheetNames() {
  const sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  sheets.forEach(sheet => Logger.log(sheet.getName()));
}
```

In der überarbeiteten Version haben wir zu `const` für Variablen gewechselt, die sich nicht ändern, was unsere Absicht klarer macht. Wir haben auch die `forEach` Methode verwendet, ein modernerer und prägnanterer Ansatz zum Durchlaufen von Arrays, der die Lesbarkeit verbessert.

**Beispielausgabe (für beide Skripte):**

Die Ausgabe im Logger sieht ungefähr so aus, vorausgesetzt, Ihr Google Sheets-Dokument hat zwei Blätter mit den Namen "Expenses" und "Revenue":

```
[20-04-2023 10:00:00: INFO] Expenses
[20-04-2023 10:00:01: INFO] Revenue
```

Das überarbeitete Skript erreicht dasselbe Ergebnis, ist aber sauberer und auf den ersten Blick leichter zu verstehen.

## Tiefere Einblicke

Das Refactoring in Google Apps Script übernimmt teilweise seine Prinzipien aus der breiteren Softwareentwicklungspraxis. Es wurde Ende der 1990er Jahre durch Martin Fowlers wegweisendes Buch "Refactoring: Improving the Design of Existing Code" (1999) bekannter und strukturierter, das einen umfassenden Leitfaden zu verschiedenen Refactoring-Techniken bot. Obwohl die Spezifika des Refactorings je nach Programmiersprache aufgrund ihrer syntaktischen und funktionalen Unterschiede variieren können, bleibt das Hauptziel dasselbe: die Verbesserung des Codes ohne Änderung seines externen Verhaltens.

Im Kontext von Google Apps Script ist ein wichtiger Aspekt, den man während des Refactorings berücksichtigen sollte, die von Google auferlegten Dienstquoten und Einschränkungen. Effizient überarbeiteter Code liest sich nicht nur besser, sondern läuft auch schneller und zuverlässiger innerhalb dieser Beschränkungen. Zum Beispiel können Stapeloperationen (`Range.setValues()` anstelle von Werten, die einzeln in Zellen gesetzt werden) die Ausführungszeit und den Quotenverbrauch erheblich reduzieren.

Es ist jedoch wichtig zu beachten, dass bei bestimmten komplexen Projekten Google Apps Script aufgrund dieser Einschränkungen möglicherweise nicht ausreicht. In solchen Fällen könnte die Suche nach Alternativen wie Google Cloud Functions oder dem neueren Geschwister von Apps Script, AppSheet, eine bessere Skalierbarkeit und Funktionalität bieten.

Letztendlich ist, während das Refactoring eine kritische Fähigkeit beim Pflegen und Verbessern von Google Apps Script-Projekten darstellt, das Verständnis der Einschränkungen der Umgebung und die Berücksichtigung alternativer Lösungen genauso wichtig für die Bereitstellung von effizientem, robustem und wartbarem Code.
