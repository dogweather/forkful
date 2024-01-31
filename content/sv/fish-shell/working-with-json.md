---
title:                "Arbeta med JSON"
date:                  2024-01-19
simple_title:         "Arbeta med JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Jobba med JSON handlar om att hantera data i JavaScript Object Notation-format, ett vanligt textbaserat format för att lagra och utbyta data. Programmerare använder JSON för dess lättvikt, enkelhet att läsa och språkoberoende natur, vilket gör det perfekt för webbapplikationer och API:er.

## Hur man gör:
Fish Shell kan inte hantera JSON utan hjälp, så vi använder `jq`, ett kommandoradsverktyg för att bearbeta JSON-data.

Installera `jq`:
```fish
sudo apt-get install jq
```

Läsa in och filtrera JSON:
```fish
echo '{"namn": "Ola", "yrke": "Utvecklare"}' | jq '.namn'
```
Output:
```
"Ola"
```

Förvandla Fish-variabel till JSON med `jq`:
```fish
set person "Ola"
echo "{\"namn\": \"$person\"}" | jq .
```
Output:
```
{
  "namn": "Ola"
}
```

Iterera över JSON-array:
```fish
echo '["Linux", "Fish", "jq"]' | jq '.[]'
```
Output:
```
"Linux"
"Fish"
"jq"
```

## Fördjupning
JSON skapades i början av 2000-talet av Douglas Crockford och har blivit standard för datadelning. Alternativ som XML och YAML finns, men JSON står ut för dess kompatibilitet och tillgänglighet på webben. Fish Shell kräver externa verktyg som `jq` för JSON-manipulering, som är implementerade genom att utnyttja Fishs pipelining-funktioner och kraftfulla string-manipulering.

## Se även:
- Fish Shell officiella dokumentation: https://fishshell.com/docs/current/index.html
- `jq` officiella dokumentation: https://stedolan.github.io/jq/
- Lär dig mer om JSON-formatet: https://www.json.org/json-en.html
