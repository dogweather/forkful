---
title:                "Arbeta med json"
html_title:           "Python: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att arbeta med JSON i Python innebär att hantera en typ av dataformat som är vanligt inom programmering. JSON står för "JavaScript Object Notation" och används för att strukturera data på ett enkelt och läsbart sätt. Programmerare använder JSON för att lagra och utbyta data mellan olika program och plattformar.

## Hur gör man:

För att använda JSON i Python behöver du importera inbyggda modulen `json`. Därefter kan du använda funktionen `load()` för att läsa in JSON-data från en fil eller en URL. Om du vill skapa en JSON-sträng från en lista eller ett dictionary, använd funktionen `dumps()`.

```
import json

# Läs in JSON-data från fil
with open('data.json') as f:
    data = json.load(f)

# Konvertera en lista till JSON-sträng
my_list = ['a', 'b', 'c']
json_str = json.dumps(my_list)
```

## Deep Dive:

Historiskt sett har JSON utvecklats för att ersätta XML som standard för datautbyte på webben. Ett alternativ till JSON är YAML, som har liknande syntax men är mer läsbar för människor. Implementationsdetaljer för JSON i Python finns på den officiella dokumentationen [här](https://docs.python.org/3/library/json.html).

## Se även:
