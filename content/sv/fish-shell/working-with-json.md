---
title:                "Fish Shell: Arbeta med JSON"
simple_title:         "Arbeta med JSON"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Varför

Att arbeta med JSON är en viktig färdighet för alla som arbetar med Fish Shell programmering. JSON används för att strukturera och överföra data på ett enkelt och lättläst sätt. Det är också ett populärt format för APIer och webbtjänster.

## Hur man gör

För att arbeta med JSON i Fish Shell, behöver vi först ladda ner ett plugin som heter "fish-json". För att göra detta, skriv:

```Fish Shell
fisher install tidusjar/fish-json
```

När pluginet har installerats, kan vi använda kommandoraden "jq" för att hantera JSON-data. Till exempel, om vi vill skriva ut en lista av element med dess attribut från en JSON-fil, kan vi göra det genom att skriva:

```Fish Shell
jq '.items[]."attribut" lista.json
```

Det här kommer att skriva ut en lista av attribut från filen "lista.json".

## Djupdykning

När vi arbetar med komplexa JSON-datastrukturer, kan det vara användbart att kunna anpassa våra utmatningar. Detta kan göras genom att använda jq's "select" och "map" funktioner.

```Fish Shell
jq '.items[].name | select(.age > 18) | map("Namn: " + .namn + " Ålder: " + (.ålder | tostring)) lista.json
```

Det här kommer att skriva ut namn och ålder på alla personer i listan som är över 18 år.

## Se även

- [Officiell Fish Shell dokumentation för JSON](https://fishshell.com/docs/current/cmds/json.html)
- [Fish JSON plugin](https://github.com/tidusjar/fish-json)
- [Officiell jq dokumentation](https://stedolan.github.io/jq/)