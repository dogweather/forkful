---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:07.839559-07:00
description: "Das Abrufen des aktuellen Datums in TypeScript, einer auf JavaScript\
  \ aufbauenden Sprache, erm\xF6glicht den Zugriff auf und die Manipulation von aktuellen\u2026"
lastmod: '2024-03-13T22:44:53.641273-06:00'
model: gpt-4-0125-preview
summary: "Das Abrufen des aktuellen Datums in TypeScript, einer auf JavaScript aufbauenden\
  \ Sprache, erm\xF6glicht den Zugriff auf und die Manipulation von aktuellen\u2026"
title: Den aktuellen Datum abrufen
weight: 29
---

## Was & Warum?
Das Abrufen des aktuellen Datums in TypeScript, einer auf JavaScript aufbauenden Sprache, ermöglicht den Zugriff auf und die Manipulation von aktuellen Datums- und Zeitinformationen. Programmierer benötigen diese Funktionalität oft, um Zeitstempel zu erstellen, Planungen vorzunehmen und andere zeitkritische Funktionen in ihren Anwendungen zu implementieren.

## Wie geht das:
In TypeScript können Sie das `Date`-Objekt verwenden, um das aktuelle Datum und die aktuelle Zeit zu erhalten. So können Sie das machen:

```typescript
const currentDate = new Date();
console.log(currentDate);
```

Beispielausgabe:
```
2023-04-12T07:20:50.52Z
```

Dieser Code-Schnipsel erstellt ein neues `Date`-Objekt, das das aktuelle Datum und die Uhrzeit enthält, welches dann in der Konsole ausgegeben wird. Sie können das Datum auch mit toLocaleDateString() formatieren, um es lesbarer zu machen:

```typescript
const currentDate = new Date();
console.log(currentDate.toLocaleDateString());
```

Beispielausgabe:
```
12.4.2023
```

### Verwendung von date-fns
Für umfangreichere Datumsmanipulationen und -formatierungen ist die Bibliothek `date-fns` eine beliebte Wahl. Installieren Sie sie zuerst über npm:

```bash
npm install date-fns
```

Anschließend können Sie sie verwenden, um das aktuelle Datum zu formatieren:

```typescript
import { format } from 'date-fns';

const currentDate = new Date();
console.log(format(currentDate, 'yyyy-MM-dd'));
```

Beispielausgabe:
```
2023-04-12
```

Dieses `date-fns`-Beispiel formatiert das aktuelle Datum als Zeichenkette im Format "JJJJ-MM-TT". Die Bibliothek bietet eine Fülle von Funktionen für die Datumsmanipulation und macht sie zu einem vielseitigen Werkzeug für jeden TypeScript-Programmierer, der mit Daten arbeitet.
