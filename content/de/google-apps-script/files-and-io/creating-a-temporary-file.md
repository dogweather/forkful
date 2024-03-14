---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:51:43.038471-07:00
description: "Das Erstellen einer tempor\xE4ren Datei in Google Apps Script beinhaltet\
  \ die Generierung einer Datei, die f\xFCr den kurzfristigen Gebrauch vorgesehen\
  \ ist,\u2026"
lastmod: '2024-03-13T22:44:53.355103-06:00'
model: gpt-4-0125-preview
summary: "Das Erstellen einer tempor\xE4ren Datei in Google Apps Script beinhaltet\
  \ die Generierung einer Datei, die f\xFCr den kurzfristigen Gebrauch vorgesehen\
  \ ist,\u2026"
title: "Erstellung einer tempor\xE4ren Datei"
---

{{< edit_this_page >}}

## Was & Warum?

Das Erstellen einer temporären Datei in Google Apps Script beinhaltet die Generierung einer Datei, die für den kurzfristigen Gebrauch vorgesehen ist, typischerweise für die Zwischenspeicherung von Daten, Debugging oder Cache-Zwecke. Programmierer tun dies, um Daten vorübergehend zu verwalten, ohne den permanenten Speicherplatz zu überladen oder wenn die Beständigkeit der Daten über den Umfang des aktuellen Prozesses hinaus unnötig ist.

## Wie geht das:

In Google Apps Script kann eine temporäre Datei mithilfe des DriveApp-Dienstes erstellt werden, der eine einfache Methode zum Erstellen, Lesen und Löschen von Dateien in Google Drive bietet. So können Sie eine temporäre Textdatei erstellen, einige Daten darauf schreiben und sie nach Gebrauch wieder entfernen:

```javascript
function createTemporaryFile() {
  // Erstellen einer temporären Datei namens "tempFile.txt"
  var tempFile = DriveApp.createFile('tempFile.txt', 'Vorübergehender Inhalt', MimeType.PLAIN_TEXT);
  
  // Loggen der Datei-URL für Zugriff oder Debugging
  Logger.log('Temporäre Datei erstellt: ' + tempFile.getUrl());
  
  // Beispieloperation: Lesen des Dateiinhalts
  var content = tempFile.getBlob().getDataAsString();
  Logger.log('Inhalt von tempFile: ' + content);
  
  // Angenommen, die Operation ist abgeschlossen und die Datei wird nicht mehr benötigt
  // Die temporäre Datei entfernen
  tempFile.setTrashed(true);
  
  // Löschung bestätigen
  Logger.log('Temporäre Datei gelöscht');
}
```

Die Ausführung dieses Skripts würde ausgeben:

```
Temporäre Datei erstellt: [URL der erstellten temporären Datei]
Inhalt von tempFile: Vorübergehender Inhalt
Temporäre Datei gelöscht
```

Dieses Beispiel-Skript zeigt die Erstellung einer temporären Datei, das Ausführen einer Operation zum Lesen ihres Inhalts und letztendlich das Entfernen der Datei zur Bereinigung.

## Vertiefung

Das Konzept der Erstellung temporärer Dateien in der Softwareentwicklung ist so alt wie das Konzept der Dateiverwaltung selbst. In traditionellen Dateisystemen werden temporäre Dateien oft in dafür vorgesehenen Temp-Verzeichnissen erstellt und sind entscheidend für verschiedene Zwischenprozesse, wie das Sortieren großer Datensätze, das Halten von Sitzungsdaten für Webanwendungen oder das Speichern von Datenfragmenten während Dateikonvertierungsprozessen.

In Google Apps Script nutzt der Prozess der Erstellung temporärer Dateien die Infrastruktur von Google Drive, was eine interessante Mischung aus cloudbasierter Dateiverwaltung mit traditionellen Programmierkonzepten bietet. Diese Methode, temporäre Dateien in Google Drive zu erstellen, hat jedoch ihre Grenzen und Kosten, wenn man die Kontingentbeschränkungen berücksichtigt, die Google Drive auferlegt. Auch die Latenz beim Zugriff auf Google Drive über das Netzwerk im Vergleich zu einem lokalen Dateisystem kann für leistungsstarke Anwendungen ein kritischer Faktor sein.

Als Alternativen könnten Entwickler in Betracht ziehen, für kleine Datensätze, die während der Berechnung eine temporäre Speicherung erfordern, Google Sheets zu verwenden, oder Google Cloud Storage für Anwendungen, die leistungsstarke Lese-/Schreiboperationen und größere Speicherkapazitäten verlangen. Jede dieser Lösungen bietet verschiedene Kompromisse hinsichtlich Latenz, Speicherlimits und Benutzerfreundlichkeit aus der Sicht von Google Apps Script. Letztendlich hängt die Wahl von den spezifischen Anforderungen der Anwendung und der bestehenden Infrastruktur ab, innerhalb derer sie arbeitet.
