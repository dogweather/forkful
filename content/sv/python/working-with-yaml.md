---
title:                "Python: Att arbeta med YAML"
simple_title:         "Att arbeta med YAML"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

YAML, som står för "YAML Ain't Markup Language", är ett vanligt textbaserat format som används för att lagra datastrukturer. Det är speciellt användbart för att skapa konfigurationsfiler och läsa indata i programmen. I denna bloggpost kommer jag att dela med mig av varför du bör lära dig YAML och hur du kan använda det i dina Python-program.

## Hur man använder det

Det första steget för att använda YAML i dina Python-program är att installera en YAML-parser som stöds av Python, som till exempel "pyyaml" eller "ruamel.yaml". Sedan kan du läsa in en YAML-fil med hjälp av `load()`-funktionen från din valda parser. Till exempel:

```Python
import yaml

with open("data.yml") as f:
    data = yaml.load(f)

print(data)
```

Koden ovan kommer att läsa in innehållet från filen "data.yml" och spara det i en variabel. Om filen innehåller en giltig YAML-struktur, kommer du att få ett Python-dictionary som utmatning.

## Djupdykning

YAML är ett mycket flexibelt format som tillåter datastrukturer som listor, dictionaries, booleans, null-värden och mycket mer. Det är också möjligt att använda strukturerat YAML med hjälp av "!!python/object/"-taggen, vilket gör det möjligt att skapa egna Python-objekt direkt från YAML-filer. Dessutom stödjer YAML kommentarer, vilket gör det enkelt att dokumentera din kod.

## Se också

- [Officiell YAML-dokumentation](https://yaml.org/)
- [PyYAML-dokumentation](https://pyyaml.org/wiki/PyYAMLDocumentation)
- [Ruamel.yaml-dokumentation](https://yaml.readthedocs.io/en/latest/)