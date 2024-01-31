---
title:                "Arbeta med JSON"
date:                  2024-01-19
simple_title:         "Arbeta med JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Jobbar med JSON för att utbyta data. Det är lättläst och populärt för webbåpplikationer.

## How to:
För att hantera JSON i Bash behöver vi ett verktyg som `jq`. Här är exempel:

```Bash
echo '{"name": "Erik", "age": 30}' | jq '.name'
```
Output:
```
"Erik"
```

Ändra åldern:
```Bash
echo '{"name": "Erik", "age": 30}' | jq '.age = 31'
```
Output:
```
{
  "name": "Erik",
  "age": 31
}
```

## Deep Dive
JSON, eller JavaScript Object Notation, skapades tidigt 2000-tal. Alternativ inkluderar XML och YAML. `jq` är kraftfullt, använder streams och är skrivet i C.

## See Also
- jq Manual: https://stedolan.github.io/jq/manual/
- JSON specifikation: https://www.json.org/json-sv.html
- Bash scripting guide: https://www.gnu.org/software/bash/manual/
