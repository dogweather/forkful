---
title:                "Att arbeta med yaml"
html_title:           "TypeScript: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Arbetet med YAML är en vanlig uppgift för programmerare som behöver läsa, skriva och konfigurera datafiler i formatet. YAML står för YAML Ain't Markup Language och är en språklig tillgång för att skapa människaläsbar datajämfört med andra dataformater som är mer svåra att läsa för människor. 

## Hur man gör:
```TypeScript
// Omvandla från YAML till JSON
import YAML from 'js-yaml';

let yamlStr = 'name: Anna\nage: 28';
let obj = YAML.load(yamlStr);

console.log(obj); // Resultat: { name: 'Anna', age: 28 }

// Omvandla från JSON till YAML
import YAML from 'js-yaml';

let obj = { name: "Anna", age: 28 };
let yamlStr = YAML.dump(obj);

console.log(yamlStr); // Resultat: name: Anna age: 28
```

## Djupare dykning:
YAML skapades ursprungligen för att underlätta konfigurering av data, men har sedan dess utvecklats till en allmän databeskrivningsformat. Det finns flera alternativ till YAML som till exempel JSON och XML, men YAML är vanligt föredraget på grund av sin läsbarhet och enkelhet när det kommer till att sammanställa data. Det finns flera olika implementationer av YAML, men den vanligaste är js-yaml-biblioteket som används i exemplet ovan.

## Se även:
[Officiell YAML-hemsida](https://yaml.org/) 
[JS-YAML dokumentation](https://www.npmjs.com/package/js-yaml)