---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:34.205684-07:00
description: "Att arbeta med JSON i Bash-programmering inneb\xE4r att tolka, extrahera\
  \ och manipulera JSON-data direkt fr\xE5n kommandoraden. Programmerare g\xF6r ofta\
  \ detta f\xF6r\u2026"
lastmod: '2024-02-25T18:49:36.418758-07:00'
model: gpt-4-0125-preview
summary: "Att arbeta med JSON i Bash-programmering inneb\xE4r att tolka, extrahera\
  \ och manipulera JSON-data direkt fr\xE5n kommandoraden. Programmerare g\xF6r ofta\
  \ detta f\xF6r\u2026"
title: Arbeta med JSON
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med JSON i Bash-programmering innebär att tolka, extrahera och manipulera JSON-data direkt från kommandoraden. Programmerare gör ofta detta för att sömlöst integrera skalprogram med webb-API:er och moderna datautbytesformat, vilket gör Bash-scriptning mer kraftfull och relevant i ett JSON-tungt ekosystem.

## Hur man gör:
Bash har i sig inga inbyggda JSON-tolkningsfunktioner, men `jq` är en kraftfull kommandoradsprocessor för JSON som fyller detta gap. Så här använder du den:

**Läsa en JSON-fil:**

Exempel `data.json`:
```json
{
  "name": "Jane Doe",
  "email": "jane@example.com",
  "location": {
    "city": "New York",
    "country": "USA"
  }
}
```

För att läsa och extrahera namnet från JSON-filen:
```bash
jq '.name' data.json
```
Output:
```
"Jane Doe"
```

**Modifiera JSON-data:**

För att uppdatera staden till "Los Angeles" och skriva tillbaka till filen:
```bash
jq '.location.city = "Los Angeles"' data.json > temp.json && mv temp.json data.json
```

**Tolka JSON från en variabel:**

Om du har JSON i en Bash-variabel kan `jq` fortfarande bearbeta den:
```bash
json_string='{"name": "John Doe", "email": "john@example.com"}'
echo $json_string | jq '.name'
```
Output:
```
"John Doe"
```

**Arbete med arrayer:**

Givet en array av objekt i JSON:
```json
{
  "items": ["apple", "banana", "cherry"]
}
```

För att extrahera det andra objektet (indexering börjar på 0):
```bash
jq '.items[1]' data.json
```
Output:
```
"banana"
```

För mer komplexa operationer och filtrering har `jq` en omfattande manual och handledningar tillgängliga online, vilket gör det till ett mångsidigt verktyg för alla dina Bash/JSON-behov.
