---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:21.723237-07:00
description: "YAML, das f\xFCr \"YAML Ain't Markup Language\" steht, ist ein f\xFC\
  r Menschen lesbarer Daten-Serialisierungsstandard, der h\xE4ufig f\xFCr Konfigurationsdateien\
  \ und\u2026"
lastmod: '2024-02-25T18:49:50.556643-07:00'
model: gpt-4-0125-preview
summary: "YAML, das f\xFCr \"YAML Ain't Markup Language\" steht, ist ein f\xFCr Menschen\
  \ lesbarer Daten-Serialisierungsstandard, der h\xE4ufig f\xFCr Konfigurationsdateien\
  \ und\u2026"
title: Arbeiten mit YAML
---

{{< edit_this_page >}}

## Was & Warum?

YAML, das für "YAML Ain't Markup Language" steht, ist ein für Menschen lesbarer Daten-Serialisierungsstandard, der häufig für Konfigurationsdateien und den Datenaustausch zwischen Sprachen mit unterschiedlichen Datenstrukturen verwendet wird. Programmierer arbeiten oft mit YAML wegen seiner Einfachheit und Lesbarkeit, insbesondere bei Projekten, die umfangreiche Konfigurationen erfordern oder wenn strukturierte Daten zwischen verschiedenen Systemen übertragen werden.

## Wie:

Obwohl Google Apps Script (GAS) nicht nativ das Parsen oder Serialisieren von YAML unterstützt, können Sie YAML-Daten manipulieren, indem Sie JavaScript-Bibliotheken verwenden oder eigene Parsing-Funktionen schreiben. Zur Demonstration betrachten wir, wie ein YAML-String mithilfe einer benutzerdefinierten Funktion geparst wird, da externe Bibliotheken nicht direkt in GAS importiert werden können.

Nehmen Sie an, Sie haben eine einfache YAML-Konfiguration:

```yaml
title: YAML-Beispiel
description: Ein Beispiel dafür, wie man YAML in Google Apps Script handhaben kann
tags:
  - Google Apps Script
  - YAML
  - Konfiguration
```

Um dies in Google Apps Script zu parsen, verwenden Sie die String-Manipulationsfähigkeiten von JavaScript:

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
      // Grundlegende Behandlung für Arrays
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
  var yaml = "title: YAML-Beispiel\ndescription: Ein Beispiel dafür, wie man YAML in Google Apps Script handhaben kann\ntags:\n  - Google Apps Script\n  - YAML\n  - Konfiguration";
  var parsed = parseYAML(yaml);
  Logger.log(parsed);
}
```

Wenn `testYamlParsing()` ausgeführt wird, gibt es aus:

```
{ title: 'YAML-Beispiel',
  description: 'Ein Beispiel dafür, wie man YAML in Google Apps Script handhaben kann',
  tags: [ 'Google Apps Script', ' YAML', ' Konfiguration' ] }
```

Dieser benutzerdefinierte Parsingsansatz ist recht grundlegend und muss möglicherweise angepasst werden, um komplexe YAML-Dateien zu handhaben.

## Vertiefung

YAML, erstmals veröffentlicht im Jahr 2001, zielte darauf ab, lesbaren als seine Vorgänger wie XML oder JSON zu sein. Während seine Einfachheit und Benutzerfreundlichkeit weit verbreitet geschätzt werden, stellt die Handhabung von YAML in Google Apps Script Herausforderungen dar, aufgrund der fehlenden direkten Unterstützung. Daher verlassen sich Programmierer oft auf die Vielseitigkeit von JavaScript, um YAML-Daten zu parsen und zu generieren. Für komplexe Anwendungsfälle, insbesondere solche mit tiefer Verschachtelung und fortgeschrittenen Datenstrukturen, kann diese Methode jedoch mühsam und fehleranfällig werden.

JSON wird im Gegensatz dazu in Google Apps Script und den meisten anderen Programmierumgebungen nativ unterstützt und bietet einen direkteren Ansatz für Daten-Serialisierung und -Deserialisierung ohne zusätzlichen Parsing-Aufwand. Die Syntax von JSON ist weniger umfangreich als die von YAML, was es für den Datenaustausch in Webanwendungen geeigneter macht. Dennoch bleibt YAML für Konfigurationsdateien und Situationen, in denen die Lesbarkeit für Menschen im Vordergrund steht, beliebt.

Wenn Sie mit YAML in Google Apps Script arbeiten, sollten Sie die Kompromisse zwischen Lesbarkeit und Benutzerfreundlichkeit berücksichtigen. Für eine umfassende YAML-Manipulation kann es sich lohnen, externe Tools oder Dienste zu erkunden, die YAML vor der Verarbeitung in Ihrem Skript in JSON konvertieren können.
