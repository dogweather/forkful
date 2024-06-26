---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:56.817654-07:00
description: "Wie: Da Google Apps Script im Wesentlichen JavaScript mit Zugriff auf\
  \ Googles App-Suite ist, erfordert die direkte Arbeit mit TOML innerhalb von Google\u2026"
lastmod: '2024-03-13T22:44:53.359461-06:00'
model: gpt-4-0125-preview
summary: Da Google Apps Script im Wesentlichen JavaScript mit Zugriff auf Googles
  App-Suite ist, erfordert die direkte Arbeit mit TOML innerhalb von Google Apps Script
  ein wenig Einfallsreichtum.
title: Arbeiten mit TOML
weight: 39
---

## Wie:
Da Google Apps Script im Wesentlichen JavaScript mit Zugriff auf Googles App-Suite ist, erfordert die direkte Arbeit mit TOML innerhalb von Google Apps Script ein wenig Einfallsreichtum. Google Apps Script unterstützt das Parsen von TOML nicht nativ, aber Sie können JavaScript-Bibliotheken nutzen oder einen einfachen Parser für grundlegende Bedürfnisse schreiben.

Lassen Sie uns als Beispiel einen einfachen TOML-Konfigurationsstring parsen:

```javascript
// TOML-String
var tomlString = `
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
`;

// Eine einfache TOML-zu-JSON-Parserfunktion
function parseTOML(tomlStr) {
  var result = {};
  var currentSection = result;
  tomlStr.split(/\r?\n/).forEach(line => {
    line = line.trim();
    if (line.startsWith('[')) { // Neuer Abschnitt
      var sectionName = line.replace(/\[|\]/g, '');
      result[sectionName] = {};
      currentSection = result[sectionName];
    } else if (line) {
      var keyValue = line.split('=').map(part => part.trim());
      var key = keyValue[0];
      var value = eval(keyValue[1]); // Verwenden Sie eval für die Einfachheit; Vorsicht im Produktivcode
      currentSection[key] = value;
    }
  });
  return result;
}

// Testen des Parsers
var configObject = parseTOML(tomlString);
console.log(configObject);

```

Die Beispiel-Ausgabe von `console.log` würde einem JSON-Objekt ähneln, wodurch es einfacher wird, auf die Konfigurationseigenschaften innerhalb von Google Apps Script zuzugreifen:

```json
{
  "database": {
    "server": "192.168.1.1",
    "ports": [8001, 8001, 8002],
    "connection_max": 5000,
    "enabled": true
  }
}
```

## Vertiefung
TOML wurde von Tom Preston-Werner, einem der Gründer von GitHub, geschaffen, um menschenfreundlicher als JSON für Konfigurationsdateien zu sein, und behält gleichzeitig die Fähigkeit, eindeutig geparst zu werden. Es strebt danach, so einfach wie möglich zu sein, ein Ziel, das sich gut mit dem Ethos vieler Entwicklungsprojekte deckt, die nach Einfachheit und Lesbarkeit in ihren Codebasen streben.

Im Kontext von Google Apps Script kann die Verwendung von TOML etwas Overhead verursachen, angesichts der fehlenden direkten Unterstützung und der Notwendigkeit, es manuell oder durch Drittanbieter-Bibliotheken zu parsen. Für kleinere Projekte oder solche, die nicht tief in Googles Ökosystem integriert sind, könnten Alternativen wie JSON oder sogar einfache Schlüssel-Wert-Paar-Strukturen in Skripteigenschaften ausreichen und einfacher zu implementieren sein. Für Anwendungen jedoch, die menschenfreundliche Konfigurationsdateien priorisieren und sich bereits auf TOML festgelegt haben, fügt die Integration des TOML-Parsings durch benutzerdefinierte Skripte eine nützliche Flexibilität und Wartbarkeit hinzu, ohne von den bevorzugten Konfigurationsparadigmen abzuweichen.
