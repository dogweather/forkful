---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:05.386295-07:00
description: "Das Suchen und Ersetzen von Text in Google Apps Script beinhaltet das\
  \ programmatische Identifizieren spezifischer Zeichenketten in einem Dokument, einer\u2026"
lastmod: '2024-03-13T22:44:53.317524-06:00'
model: gpt-4-0125-preview
summary: "Das Suchen und Ersetzen von Text in Google Apps Script beinhaltet das programmatische\
  \ Identifizieren spezifischer Zeichenketten in einem Dokument, einer\u2026"
title: Suchen und Ersetzen von Text
---

{{< edit_this_page >}}

## Was & Warum?

Das Suchen und Ersetzen von Text in Google Apps Script beinhaltet das programmatische Identifizieren spezifischer Zeichenketten in einem Dokument, einer Tabelle oder einem anderen Typ von Google-Apps-Inhalten und das Austauschen dieser mit anderen Textwerten. Programmierer nutzen diese Funktionalität, um das Bearbeiten großer Mengen von Inhalten zu automatisieren, gängige Fehler zu korrigieren, Terminologie in Dokumenten zu standardisieren oder dynamische Daten in Vorlagen einzufügen.

## Wie geht das:

Google Apps Script bietet eine unkomplizierte Möglichkeit, Text zu suchen und zu ersetzen, insbesondere in Google Docs und Sheets. Im Folgenden sind Beispiele für beide.

### Google Docs:

Um Text in einem Google-Dokument zu suchen und zu ersetzen, interagierst du hauptsächlich mit der `DocumentApp`-Klasse.

```javascript
function searchReplaceInDoc() {
  var doc = DocumentApp.getActiveDocument();
  var body = doc.getBody();
  
  // Um eine spezifische Phrase zu suchen und zu ersetzen
  body.replaceText('searchText', 'replacementText');
  
  DocumentApp.getActiveDocument().saveAndClose();
}

// Verwendung
searchReplaceInDoc();
```

Dieser Codeausschnitt sucht nach allen Vorkommen von `'searchText'` im aktiven Google-Dokument und ersetzt diese durch `'replacementText'`.

### Google Sheets:

Ähnlich kannst du in Google Sheets `SpreadsheetApp` verwenden, um Such- und Ersetzungsoperationen durchzuführen:

```javascript
function searchReplaceInSheet() {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  
  // Suchen und Ersetzen im aktuell aktiven Blatt
  // replaceText(searchText, replacement)
  sheet.createTextFinder('searchText').replaceAllWith('replacementText');
}

// Verwendung
searchReplaceInSheet();
```

In diesem Beispiel sucht `createTextFinder('searchText')` im aktiven Blatt nach 'searchText', und `replaceAllWith('replacementText')` ersetzt alle Vorkommen durch 'replacementText'.

## Tiefergehend

Die Such- und Ersetzungsfunktionalität in Google Apps Script wird stark von seiner webbasierten Natur beeinflusst und ermöglicht es Skripten, Texte nahtlos in verschiedenen Google-Apps zu manipulieren. Historisch gesehen stammt diese Fähigkeit aus dem breiteren Kontext der Textverarbeitung und -manipulation in der Programmierung, wo reguläre Ausdrücke und String-Funktionen in Sprachen wie Perl und Python einen hohen Standard an Flexibilität und Leistung setzten.

Obwohl die Such- und Ersetzungsfunktionalität in Google Apps Script für einfache Substitutionen leistungsfähig ist, fehlt es ihr an den vollen Fähigkeiten regulärer Ausdrücke, die in einigen anderen Sprachen gefunden werden können. Zum Beispiel, während du in `createTextFinder` in Google Sheets grundlegende reguläre Ausdrücke verwenden kannst, sind die Optionen für komplexe Musterabgleichung und -manipulation im Vergleich zu Perl oder Python eingeschränkt.

Für fortgeschrittenere Textverarbeitungsbedürfnisse könnten Programmierer dazu übergehen, den Google-Docs- oder Sheets-Inhalt in ein Format zu exportieren, das extern mit leistungsfähigeren Sprachen verarbeitet werden kann, oder Google Apps Script verwenden, um externe APIs oder Dienste aufzurufen, die ausgefeiltere Textmanipulationsfähigkeiten bieten.

Trotz dieser Einschränkungen bietet Google Apps Script für die meisten typischen Such- und Ersetzungsaufgaben innerhalb des Ökosystems von Google-Apps eine einfache, effiziente und hochintegrierbare Lösung, die auf die Bedürfnisse der Automatisierung und Skripterstellung innerhalb von Googles Suite von Produktivitätswerkzeugen zugeschnitten ist.
