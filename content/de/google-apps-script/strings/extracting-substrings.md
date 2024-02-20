---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:48.269883-07:00
description: "Das Extrahieren von Teilzeichenfolgen beinhaltet das Nehmen eines Teils\
  \ einer Zeichenkette - im Wesentlichen das Erstellen einer neuen Zeichenkette aus\u2026"
lastmod: 2024-02-19 22:05:12.368182
model: gpt-4-0125-preview
summary: "Das Extrahieren von Teilzeichenfolgen beinhaltet das Nehmen eines Teils\
  \ einer Zeichenkette - im Wesentlichen das Erstellen einer neuen Zeichenkette aus\u2026"
title: Teilzeichenketten extrahieren
---

{{< edit_this_page >}}

## Was & Warum?

Das Extrahieren von Teilzeichenfolgen beinhaltet das Nehmen eines Teils einer Zeichenkette - im Wesentlichen das Erstellen einer neuen Zeichenkette aus einem Teil einer bestehenden. Programmierer tun dies aus einer Vielzahl von Gründen, einschließlich Daten- Parsing, Textmanipulation für Benutzeroberflächen oder Verarbeitung von Eingaben für verschiedene Anwendungen, was das Extrahieren von Teilzeichenfolgen zu einem vielseitigen Werkzeug in jedem Skriptarsenal macht.

## Wie:

In Google Apps Script, das auf modernem JavaScript basiert, kann das Extrahieren von Teilzeichenfolgen durch mehrere Methoden erreicht werden, einschließlich `substring()`, `substr()` und `slice()`. Jede hat ihre Nuancen, aber alle dienen dem Zweck, bestimmte Zeichen aus einer Zeichenkette zu ziehen.

```javascript
// Beispiel mit substring()
var str = "Hallo, Welt!";
var result = str.substring(0, 5);
console.log(result); // Ausgabe: Hallo

// Beispiel mit substr()
var resultSubstr = str.substr(7, 5);
console.log(resultSubstr); // Ausgabe: Welt

// Beispiel mit slice()
var resultSlice = str.slice(-6);
console.log(resultSlice); // Ausgabe: Welt!
```

Jede Methode nimmt zwei Argumente: die Startposition und, außer bei `slice()`, das auch negative Indizes akzeptieren kann, um vom Ende aus zu starten, die Endposition oder die Anzahl der Zeichen, die extrahiert werden sollen. Es ist bemerkenswert, dass die ursprüngliche Zeichenkette nach diesen Operationen unverändert bleibt, da sie neue Zeichenkettenwerte zurückgeben.

## Tiefergehende Betrachtung

Historisch gesehen waren die JavaScript-Methoden zum Extrahieren von Teilzeichenfolgen aufgrund ihrer ähnlichen Namen und Funktionalitäten eine Quelle der Verwirrung. In Google Apps Script und modernem JavaScript werden jedoch `substring()` und `slice()` am häufigsten verwendet, wobei `substr()` als veraltet angesehen wird. Dies ist wichtig zu beachten für diejenigen, die zukunftssicheren Code schreiben.

Der Hauptunterschied zwischen `substring()` und `slice()` liegt darin, wie sie negative Indizes behandeln; `substring()` behandelt negative Indizes als 0, während `slice()` einen negativen Index akzeptieren kann, um die Extraktion vom Ende der Zeichenkette zu beginnen. Dies macht `slice()` besonders praktisch für Fälle, in denen die genaue Länge der Zeichenkette nicht bekannt sein könnte oder wenn eine Extraktion vom Ende notwendig ist.

Bei der Entscheidung, welche Methode für die Extraktion von Teilzeichenfolgen verwendet werden soll, hängt die Wahl oft von den spezifischen Anforderungen der Operation ab (z. B., ob die Handhabung negativer Indizes vorteilhaft ist) und persönlichen oder Team-Codierungsstandards. Obwohl es keine Einheitslösung als beste Praxis gibt, kann das Verständnis der subtilen Unterschiede und der Leistungsauswirkungen helfen, eine informierte Entscheidung zu treffen.
