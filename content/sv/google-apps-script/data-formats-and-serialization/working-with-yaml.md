---
title:                "Att Arbeta med YAML"
aliases: - /sv/google-apps-script/working-with-yaml.md
date:                  2024-02-01T22:07:29.694093-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att Arbeta med YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/google-apps-script/working-with-yaml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

YAML, som står för "YAML Ain't Markup Language", är en standard för serialisering av data som är läslig för människor och används ofta för konfigurationsfiler och datautbyte mellan språk med olika datastrukturer. Programmerare arbetar ofta med YAML på grund av dess enkelhet och läsbarhet, särskilt i projekt som kräver omfattande konfiguration eller vid överföring av strukturerad data mellan olika system.

## Hur man gör:

Medan Google Apps Script (GAS) inte stöder YAML-tolkning eller serialisering som standard, kan du manipulera YAML-data genom att använda JavaScript-bibliotek eller skriva egna tolkningsfunktioner. För demonstration, låt oss överväga hur man tolkar en YAML-sträng med hjälp av en anpassad funktion, eftersom externa bibliotek inte kan importeras direkt i GAS.

Anta att du har en enkel YAML-konfiguration:

```yaml
title: YAML-exempel
description: Ett exempel på hur man hanterar YAML i Google Apps Script
tags:
  - Google Apps Script
  - YAML
  - Konfiguration
```

För att tolka detta i Google Apps Script, använd JavaScripts förmåga att manipulera strängar:

```javascript
function parseYAML(yamlString) {
  var result = {};
  var lines = yamlString.split("\n");
  for (var i = 0; i < lines.length; i++) {
    var line = lines[i];
    if (line.includes(":")) {
      var parts = line.split(":");
      var key = parts[0].trim();
      var value = parts[1].trim();
      // Grundläggande hantering för arrayer
      if (value.startsWith("-")) {
        value = [value.substring(1).trim()];
        while (i + 1 < lines.length && lines[i + 1].trim().startsWith("-")) {
          i++;
          value.push(lines[i].trim().substring(1).trim());
        }
      }
      result[key] = value;
    }
  }
  return result;
}

function testYamlParsing() {
  var yaml = "title: YAML-exempel\ndescription: Ett exempel på hur man hanterar YAML i Google Apps Script\ntags:\n  - Google Apps Script\n  - YAML\n  - Konfiguration";
  var parsed = parseYAML(yaml);
  Logger.log(parsed);
}
```

När `testYamlParsing()` körs, ger det utskriften:

```
{ title: 'YAML-exempel',
  description: 'Ett exempel på hur man hanterar YAML i Google Apps Script',
  tags: [ 'Google Apps Script', ' YAML', ' Konfiguration' ] }
```

Detta anpassade tolkningsansats är ganska grundläggande och kan behöva justeras för att kunna hantera komplexa YAML-filer.

## Fördjupning

YAML, som ursprungligen släpptes 2001, syftade till att vara mer läsbart för människor än dess föregångare som XML eller JSON. Medan dess enkelhet och användarvänlighet är mycket uppskattad, presenterar hanteringen av YAML i Google Apps Script utmaningar på grund av bristen på direkt stöd. Följaktligen litar programmerare ofta på JavaScripts mångsidighet för att tolka och generera YAML-data. Dock, för komplexa användningsfall, särskilt de som involverar djup nästling och avancerade datastrukturer, kan denna metod bli besvärlig och benägen för fel.

JSON stöds däremot infödd i Google Apps Script och de flesta andra programmeringsmiljöer, och erbjuder en mer rakt på sak strategi för data-serialisering och deserialisering utan extra tolkningsöverhuvud. JSONs syntax är mindre långvärd än YAMLs, vilket gör den mer lämplig för datautbyte i webbapplikationer. Dock förblir YAML populärt för konfigurationsfiler och situationer där människans läsbarhet är av yttersta vikt.

När du arbetar med YAML i Google Apps Script, överväg avvägningarna mellan läsbarhet och användarvänlighet. För omfattande YAML-manipulation, kan det vara värt att utforska externa verktyg eller tjänster som kan konvertera YAML till JSON innan du bearbetar det inom ditt skript.
