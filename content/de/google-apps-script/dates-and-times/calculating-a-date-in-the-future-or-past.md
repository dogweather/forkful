---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:48:51.088441-07:00
description: "Die Berechnung eines Datums in der Zukunft oder der Vergangenheit handelt\
  \ vom Manipulieren von Datumsobjekten, um Daten \xFCber das gegenw\xE4rtige Datum\
  \ hinaus\u2026"
lastmod: '2024-03-13T22:44:53.348746-06:00'
model: gpt-4-0125-preview
summary: "Die Berechnung eines Datums in der Zukunft oder der Vergangenheit handelt\
  \ vom Manipulieren von Datumsobjekten, um Daten \xFCber das gegenw\xE4rtige Datum\
  \ hinaus oder davor zu finden."
title: "Berechnung eines zuk\xFCnftigen oder vergangenen Datums"
weight: 26
---

## Wie:
In Google Apps Script, das auf JavaScript basiert, können Sie Daten mit dem `Date`-Objekt manipulieren. So berechnen Sie Daten in der Zukunft und in der Vergangenheit:

### Zukünftiges Datum berechnen
Um ein zukünftiges Datum zu berechnen, erstellen Sie ein Datum-Objekt für das aktuelle Datum und fügen dann die gewünschte Anzahl von Tagen (oder anderen Zeiteinheiten) hinzu.

```javascript
// Aktuelles Datum
var today = new Date();

// Ein Datum 10 Tage in der Zukunft berechnen
var futureDate = new Date(today);
futureDate.setDate(today.getDate() + 10);

Logger.log("Zukünftiges Datum: " + futureDate.toDateString());
```

### Vergangenes Datum berechnen
Ähnlich, um ein Datum in der Vergangenheit zu finden, subtrahieren Sie die Anzahl der Tage vom aktuellen Datum.

```javascript
// Aktuelles Datum
var today = new Date();

// Ein Datum 10 Tage in der Vergangenheit berechnen
var pastDate = new Date(today);
pastDate.setDate(today.getDate() - 10);

Logger.log("Vergangenes Datum: " + pastDate.toDateString());
```

### Beispiel-Ausgabe
Dies würde etwas wie das Folgende ausgeben (angenommen, heute ist der 15. April 2023):

```
Zukünftiges Datum: Di Apr 25 2023
Vergangenes Datum: Mi Apr 05 2023
```

Denken Sie daran, dass das Date-Objekt in JavaScript (und somit in Google Apps Script) Monate und Jahre automatisch anpasst, wenn Sie Tage hinzufügen oder subtrahieren.

## Tiefergehend
Die Manipulation von Daten mit dem `Date`-Objekt stammt aus frühen JavaScript-Implementierungen. Im Laufe der Zeit ist dieser Ansatz im Allgemeinen konsistent geblieben und bietet eine unkomplizierte Möglichkeit für Entwickler, mit Daten zu arbeiten, ohne externe Bibliotheken zu benötigen. Allerdings können für komplexere Operationen wie Zeitzoneanpassungen oder die Arbeit mit umfangreichen datumsbasierten Daten Bibliotheken wie `Moment.js` oder das modernere `Luxon` mehr Funktionalität und einfachere Handhabung bieten.

Speziell in Google Apps Script, trotz der direkten Verfügbarkeit und Einfachheit des `Date`-Objekts, ist es entscheidend, darauf zu achten, wie Datumsberechnungen die Skriptleistung und Ausführungszeit beeinflussen können, insbesondere bei zeitgesteuerten Auslösern oder umfangreichen Tabellenkalkulationen. Außerdem, obwohl Google Apps Script integrierte Methoden bietet, um mit Daten in seinem Ökosystem umzugehen (wie in Google Sheets oder Kalender), kann die Integration externer Bibliotheken oder die Nutzung der erweiterten Dienste von Google manchmal robustere Lösungen für komplexe Szenarien bieten.

Somit ist, während die native JavaScript `Date`-Objekt Methodik normalerweise ausreichend für einfache Berechnungen ist, das Erkunden externer Bibliotheken oder Dienste kann die Funktionalität für nuanciertere Anforderungen erweitern.
