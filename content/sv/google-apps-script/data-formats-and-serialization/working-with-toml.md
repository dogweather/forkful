---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:12.884155-07:00
description: "TOML, som st\xE5r f\xF6r Toms Uppenbara, Minimala Spr\xE5k, \xE4r ett\
  \ konfigurationsfilformat som \xE4r l\xE4tt att l\xE4sa p\xE5 grund av dess tydliga\
  \ semantik. Programmerare\u2026"
lastmod: '2024-03-13T22:44:37.464381-06:00'
model: gpt-4-0125-preview
summary: "TOML, som st\xE5r f\xF6r Toms Uppenbara, Minimala Spr\xE5k, \xE4r ett konfigurationsfilformat\
  \ som \xE4r l\xE4tt att l\xE4sa p\xE5 grund av dess tydliga semantik."
title: Arbeta med TOML
weight: 39
---

## Hur man gör:
Eftersom Google Apps Script i huvudsak är JavaScript med tillgång till Googles appsvit, kräver arbete med TOML direkt inom Google Apps Script lite uppfinningsrikedom. Google Apps Script stöder inte inbyggd parsing av TOML, men du kan använda JavaScript-bibliotek eller skriva en enkel parser för grundläggande behov.

Låt oss tolka en enkel TOML-konfigurationssträng som ett exempel:

```javascript
// TOML-sträng
var tomlString = `
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
`;

// En enkel TOML till JSON parserfunktion
function parseTOML(tomlStr) {
  var result = {};
  var currentSection = result;
  tomlStr.split(/\r?\n/).forEach(line => {
    line = line.trim();
    if (line.startsWith('[')) { // Ny sektion
      var sectionName = line.replace(/\[|\]/g, '');
      result[sectionName] = {};
      currentSection = result[sectionName];
    } else if (line) {
      var keyValue = line.split('=').map(part => part.trim());
      var key = keyValue[0];
      var value = eval(keyValue[1]); // Använd eval för enkelhet; var försiktig i produktionskod
      currentSection[key] = value;
    }
  });
  return result;
}

// Testa parsern
var configObject = parseTOML(tomlString);
console.log(configObject);

```

Exempelutdata från `console.log` skulle likna ett JSON-objekt, vilket gör det lättare att få tillgång till konfigurationsattributen inom Google Apps Script:

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

## Djupdykning
TOML skapades av Tom Preston-Werner, en av grundarna av GitHub, för att vara mer människovänligt än JSON för konfigurationsfiler samtidigt som det kan tolkas otvetydigt. Målet är att vara så enkelt som möjligt, ett mål som ligger väl i linje med etoset hos många utvecklingsprojekt som strävar efter enkelhet och läsbarhet i sina kodbasar.

I kontexten av Google Apps Script, kan användningen av TOML introducera viss overhead, med tanke på bristen på direkt stöd och nödvändigheten att tolka det manuellt eller genom tredjepartsbibliotek. För mindre projekt eller de som inte är djupt integrerade i Googles ekosystem, kan alternativ som JSON eller till och med enkla nyckel-värdeparstrukturer i skriptegenskaper räcka och vara enklare att implementera. Men för applikationer som prioriterar människovänliga konfigurationsfiler och redan är åtagna till TOML, lägger integrationen av TOML-parsing genom anpassade skript till ett användbart lager av flexibilitet och underhållbarhet utan att avvika från de föredragna konfigurationsparadigmen.
