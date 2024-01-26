---
title:                "Arbete med YAML"
html_title:           "Arduino: Arbete med YAML"
simple_title:         "Arbete med YAML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad och Varför?
YAML är ett dataformat som används för konfigurationsfiler och datautbyte. Programmerare använder det för dess läslighet och enkelhet, särskilt för att hantera hierarkiska data eller konfigurationer.

## Hur gör man:
Använd npm-paketet `js-yaml` för att enkelt hantera YAML i Javascript.

```javascript
const yaml = require('js-yaml');
const fs = require('fs');

// Läs YAML från en fil
const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
console.log(doc);

// Konvertera Javascript-objekt till YAML och skriv till fil
const data = { title: 'Hej', numbers: [1, 2, 3] };
const yamlStr = yaml.dump(data);
fs.writeFileSync('./config.yaml', yamlStr);
```

Om `config.yaml` ser ut så här:

```yaml
title: Hej
numbers:
  - 1
  - 2
  - 3
```

Kommer output att vara:

```
{ title: 'Hej', numbers: [ 1, 2, 3 ] }
```

## Deep Dive
YAML, som står för "YAML Ain't Markup Language", introducerades i början av 2000-talet som ett alternativ till XML. Fördelen är dess enkelhet och tillgänglighet för människor. JSON är ett vanligt alternativ, men YAML tillåter kommentarer och kan bättre hantera komplexa strukturer. När det gäller implementering kan YAML användas på servern eller i front-end, men det är viktigt att bearbeta de inhämtade datorna för att förhindra säkerhetsrisker som kan uppstå med skadlig YAML.

## See Also
- YAML officiell webbplats: [https://yaml.org](https://yaml.org)
- js-yaml GitHub repo: [https://github.com/nodeca/js-yaml](https://github.com/nodeca/js-yaml)
- YAML vs JSON: [https://json2yaml.com/](https://json2yaml.com/)
- YAML-syntaxguide: [https://learnxinyminutes.com/docs/yaml/](https://learnxinyminutes.com/docs/yaml/)
