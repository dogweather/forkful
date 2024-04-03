---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:14.860239-07:00
description: "TOML, co oznacza Tom's Obvious, Minimal Language (Oczywisty, Minimalny\
  \ J\u0119zyk Toma), to format pliku konfiguracyjnego, kt\xF3ry jest \u0142atwy do\
  \ odczytania\u2026"
lastmod: '2024-03-13T22:44:34.929023-06:00'
model: gpt-4-0125-preview
summary: "TOML, co oznacza Tom's Obvious, Minimal Language (Oczywisty, Minimalny J\u0119\
  zyk Toma), to format pliku konfiguracyjnego, kt\xF3ry jest \u0142atwy do odczytania\
  \ dzi\u0119ki swojej jasnej semantyce."
title: Praca z TOML
weight: 39
---

## Co i dlaczego?

TOML, co oznacza Tom's Obvious, Minimal Language (Oczywisty, Minimalny Język Toma), to format pliku konfiguracyjnego, który jest łatwy do odczytania dzięki swojej jasnej semantyce. Programiści często używają go do plików konfiguracyjnych w aplikacjach, ponieważ jest prosty i czytelny dla człowieka, co ułatwia zarządzanie ustawieniami i konfiguracjami aplikacji w różnych środowiskach.

## Jak to zrobić:

Ponieważ Google Apps Script to w zasadzie JavaScript z dostępem do zestawu aplikacji Google'a, praca z TOML bezpośrednio w Google Apps Script wymaga trochę pomysłowości. Google Apps Script nie obsługuje natywnie parsowania TOML, ale można wykorzystać biblioteki JavaScript lub napisać prosty parser do podstawowych potrzeb.

Oto przykład parsowania prostej konfiguracji TOML jako przykład:

```javascript
// Ciąg TOML
var tomlString = `
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
`;

// Prosta funkcja parsera TOML na JSON
function parseTOML(tomlStr) {
  var result = {};
  var currentSection = result;
  tomlStr.split(/\r?\n/).forEach(line => {
    line = line.trim();
    if (line.startsWith('[')) { // Nowa sekcja
      var sectionName = line.replace(/\[|\]/g, '');
      result[sectionName] = {};
      currentSection = result[sectionName];
    } else if (line) {
      var keyValue = line.split('=').map(part => part.trim());
      var key = keyValue[0];
      var value = eval(keyValue[1]); // Używamy eval dla uproszczenia; uważaj w kodzie produkcyjnym
      currentSection[key] = value;
    }
  });
  return result;
}

// Test parsera
var configObject = parseTOML(tomlString);
console.log(configObject);

```

Przykładowy wynik `console.log` przypominałby obiekt JSON, co ułatwia dostęp do właściwości konfiguracyjnych w Google Apps Script:

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

## Pogłębiona analiza

TOML został stworzony przez Toma Preston-Wernera, jednego z założycieli GitHuba, aby był bardziej przyjazny dla człowieka niż JSON dla plików konfiguracyjnych, jednocześnie zachowując zdolność do jednoznacznego parsowania. Ma być tak prosty, jak to tylko możliwe, cel, który ładnie wpisuje się w etos wielu projektów programistycznych dążących do prostoty i czytelności w ich bazach kodu.

W kontekście Google Apps Script, używanie TOML może wprowadzić pewne obciążenie, biorąc pod uwagę brak bezpośredniego wsparcia i konieczność ręcznego parsowania lub poprzez biblioteki stron trzecich. Dla mniejszych projektów lub tych niezbyt głęboko zintegrowanych z ekosystemem Google'a, alternatywy takie jak JSON lub nawet proste struktury par klucz-wartość w właściwościach skryptu mogą wystarczyć i być łatwiejsze do wdrożenia. Jednakże, dla aplikacji, które stawiają na przyjazne dla człowieka pliki konfiguracyjne i są już zaangażowane w TOML, integracja parsowania TOML przez niestandardowe skrypty dodaje użyteczną warstwę elastyczności i możliwość utrzymania bez odchodzenia od preferowanych paradygmatów konfiguracyjnych.
