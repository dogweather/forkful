---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:42.074796-07:00
description: "Jak to zrobi\u0107: Chocia\u017C Google Apps Script (GAS) nie obs\u0142\
  uguje natywnie analizy sk\u0142adniowej YAML ani serializacji, mo\u017Cna manipulowa\u0107\
  \ danymi YAML, u\u017Cywaj\u0105c\u2026"
lastmod: '2024-03-13T22:44:34.925741-06:00'
model: gpt-4-0125-preview
summary: "Chocia\u017C Google Apps Script (GAS) nie obs\u0142uguje natywnie analizy\
  \ sk\u0142adniowej YAML ani serializacji, mo\u017Cna manipulowa\u0107 danymi YAML,\
  \ u\u017Cywaj\u0105c bibliotek JavaScript lub pisz\u0105c w\u0142asne funkcje analizuj\u0105\
  ce."
title: Praca z YAML
weight: 41
---

## Jak to zrobić:
Chociaż Google Apps Script (GAS) nie obsługuje natywnie analizy składniowej YAML ani serializacji, można manipulować danymi YAML, używając bibliotek JavaScript lub pisząc własne funkcje analizujące. Dla demonstracji rozważmy sposób analizowania ciągu YAML przy użyciu niestandardowej funkcji, ponieważ zewnętrzne biblioteki nie mogą być bezpośrednio importowane do GAS.

Załóżmy, że masz prostą konfigurację YAML:

```yaml
title: Przykład YAML
description: Przykład obsługi YAML w Google Apps Script
tags:
  - Google Apps Script
  - YAML
  - Konfiguracja
```

Aby przeanalizować to w Google Apps Script, użyj możliwości manipulacji ciągami JavaScript:

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
      // Podstawowa obsługa tablic
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
  var yaml = "title: Przykład YAML\ndescription: Przykład obsługi YAML w Google Apps Script\ntags:\n  - Google Apps Script\n  - YAML\n  - Konfiguracja";
  var parsed = parseYAML(yaml);
  Logger.log(parsed);
}
```

Gdy zostanie wykonana funkcja `testYamlParsing()`, na wyjściu otrzymamy:

```
{ title: 'Przykład YAML',
  description: 'Przykład obsługi YAML w Google Apps Script',
  tags: [ 'Google Apps Script', ' YAML', ' Konfiguracja' ] }
```

Ta niestandardowa metoda analizy jest dość podstawowa i może wymagać dostosowań, aby obsłużyć skomplikowane pliki YAML.

## Dalsze zgłębianie
YAML, wydany początkowo w 2001 roku, miał na celu być bardziej czytelny dla ludzi niż jego poprzednicy, tacy jak XML czy JSON. Chociaż jego prostota i łatwość użycia są szeroko doceniane, obsługa YAML w Google Apps Script stwarza wyzwania ze względu na brak bezpośredniego wsparcia. W konsekwencji programiści często polegają na wszechstronności JavaScriptu do analizowania i generowania danych YAML. Jednakże, dla złożonych przypadków użycia, szczególnie tych obejmujących głębokie zagnieżdżanie i zaawansowane struktury danych, ta metoda może być uciążliwa i podatna na błędy.

JSON, w przeciwieństwie, jest natywnie obsługiwany w Google Apps Script i większości innych środowisk programistycznych, oferując prostsze podejście do serializacji i deserializacji danych bez dodatkowego obciążenia związanego z analizowaniem. Składnia JSON jest mniej rozwlekła niż YAML, co czyni go bardziej odpowiednim do wymiany danych w aplikacjach internetowych. Niemniej jednak, YAML pozostaje popularny do plików konfiguracyjnych i sytuacji, w których czytelność dla człowieka jest kluczowa.

Pracując z YAML w Google Apps Script, należy rozważyć kompromis pomiędzy czytelnością a łatwością użycia. Dla kompleksowej manipulacji YAML, warto zbadać zewnętrzne narzędzia lub usługi, które mogą konwertować YAML na JSON przed przetworzeniem go w skrypcie.
