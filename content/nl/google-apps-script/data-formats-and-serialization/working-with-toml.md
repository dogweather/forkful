---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:14.168312-07:00
description: "Hoe te: Aangezien Google Apps Script in wezen JavaScript is met toegang\
  \ tot de suite van Google-apps, vereist werken met TOML direct binnen Google Apps\u2026"
lastmod: '2024-03-13T22:44:50.357096-06:00'
model: gpt-4-0125-preview
summary: Aangezien Google Apps Script in wezen JavaScript is met toegang tot de suite
  van Google-apps, vereist werken met TOML direct binnen Google Apps Script een beetje
  vindingrijkheid.
title: Werken met TOML
weight: 39
---

## Hoe te:
Aangezien Google Apps Script in wezen JavaScript is met toegang tot de suite van Google-apps, vereist werken met TOML direct binnen Google Apps Script een beetje vindingrijkheid. Google Apps Script ondersteunt geen TOML-parsing van nature, maar je kunt JavaScript-bibliotheken gebruiken of een eenvoudige parser schrijven voor basisbehoeften.

Laten we als voorbeeld een eenvoudige TOML-configuratiestring parsen:

```javascript
// TOML-string
var tomlString = `
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
`;

// Een eenvoudige TOML naar JSON parserfunctie
function parseTOML(tomlStr) {
  var resultaat = {};
  var huidigeSectie = resultaat;
  tomlStr.split(/\r?\n/).forEach(regel => {
    regel = regel.trim();
    if (regel.startsWith('[')) { // Nieuwe sectie
      var sectieNaam = regel.replace(/\[|\]/g, '');
      resultaat[sectieNaam] = {};
      huidigeSectie = resultaat[sectieNaam];
    } else if (regel) {
      var sleutelWaarde = regel.split('=').map(deel => deel.trim());
      var sleutel = sleutelWaarde[0];
      var waarde = eval(sleutelWaarde[1]); // Gebruik eval voor eenvoud; wees voorzichtig in productiecode
      huidigeSectie[sleutel] = waarde;
    }
  });
  return resultaat;
}

// Test de parser
var configObject = parseTOML(tomlString);
console.log(configObject);

```

Een voorbeelduitvoer van de `console.log` zou lijken op een JSON-object, wat het gemakkelijker maakt om toegang te krijgen tot de configuratie-eigenschappen binnen Google Apps Script:

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

## Diepere Duik
TOML is gecreëerd door Tom Preston-Werner, een van de oprichters van GitHub, om mensvriendelijker te zijn dan JSON voor configuratiebestanden, terwijl het nog steeds ondubbelzinnig te parsen is. Het streeft ernaar zo eenvoudig mogelijk te zijn, een doel dat mooi aansluit bij de ethos van veel ontwikkelingsprojecten die streven naar eenvoud en leesbaarheid in hun codebases.

In de context van Google Apps Script, kan het gebruik van TOML wat overhead introduceren, gezien het gebrek aan directe ondersteuning en de noodzaak om het handmatig of via externe bibliotheken te parsen. Voor kleinere projecten of die niet diep geïntegreerd zijn in het ecosysteem van Google, kunnen alternatieven zoals JSON of zelfs eenvoudige sleutel-waardepaarstructuren in scripteigenschappen volstaan en eenvoudiger te implementeren zijn. Echter, voor applicaties die mensvriendelijke configuratiebestanden prioriteren en al gecommitteerd zijn aan TOML, voegt het integreren van TOML-parsing door aangepaste scripts een nuttige laag van flexibiliteit en onderhoudbaarheid toe zonder af te wijken van de voorkeursconfiguratieparadigma's.
