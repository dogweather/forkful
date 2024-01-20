---
title:                "Arbeta med json"
html_title:           "Fish Shell: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att jobba med JSON är ett sätt för programmerare att hantera och manipulera data på ett enkelt och strukturerat sätt. JSON står för JavaScript Object Notation och är ett populärt format för att representera och utbyta data.

## Hur?
Fish Shell stödjer direkt åtkomst till JSON-data genom att använda kommandot `jq`. Det låter dig filtrera, manipulera och format HTML-data direkt i terminalen.

```fish
Fish Shell ~ $ curl 'https://swapi.dev/api/people/1' | jq '.name'
"Luke Skywalker"
```

## Fördjupning
JSON har funnits sedan 2001 och används ofta för att representera data på webben. Alternativ till JSON inkluderar XML och YAML, men JSON anses vara enklare och mer lättläst för människor.

Fish Shell implementerar stöd för JSON genom att använda en extern kommandoradverktyg `jq` som måste installeras separat. För att installera `jq` inom Fish Shell, kan du köra kommandot `fisher install ilancosman/tide`.

## Se även
- [Officiell `jq` dokumentation](https://stedolan.github.io/jq/)