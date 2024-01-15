---
title:                "Att arbeta med json"
html_title:           "Fish Shell: Att arbeta med json"
simple_title:         "Att arbeta med json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Varför

Att arbeta med JSON (JavaScript Object Notation) är ett sätt att strukturera och hantera data på ett enkelt och effektivt sätt. Det är särskilt användbart för att överföra och lagra data på webben.

## Hur man gör

För att kunna använda JSON i Fish Shell, behöver du först installera och aktivera paketet "jq" genom kommandot: `fisher add edc/bass && fisher install jethrokuan/z json`.

Sedan kan du läsa, filtrera och bearbeta JSON-data genom olika kommandon.

För att läsa en JSON-fil och visa dess innehåll:
```
jq "." sample.json
```
Output:
```
{
  "name": "John Doe",
  "age": 25,
  "hobbies": ["hiking", "cooking", "reading"]
}
```

För att få en specifik del av datan, som till exempel hobbies:
```
jq ".hobbies" sample.json
```
Output:
```
["hiking", "cooking", "reading"]
```

För att filtrera datan baserat på ett villkor, som att bara visa personer över 18 år:
```
jq "select(.age > 18)" sample.json
```
Output:
```
{
  "name": "John Doe",
  "age": 25,
  "hobbies": ["hiking", "cooking", "reading"]
}
```

## Djupdykning

Utöver grundläggande läsning och filtrering, kan man också utföra mer avancerade manipulationer av JSON-data.

En viktig funktion är att kunna konvertera JSON till tabellformat, vilket kan vara användbart för att lättare bearbeta och visualisera data. Detta kan göras med kommandot `jq -r ". | to_entries | .[] | [.key, .value] | @tsv"`.

En annan användbar funktion är att kunna sammanfoga flera JSON-filer till en enda fil, vilket kan underlätta hanteringen av större datamängder. Detta kan göras genom att ange sökvägen till varje JSON-fil som argument efter pipet, till exempel `jq ". | add (.[] | select (. != \"\"))" file1.json file2.json`.

För att lära dig mer om hur man arbetar med JSON i Fish Shell, besök gärna den officiella dokumentationen för JQ (https://stedolan.github.io/jq/).

## Se även

- JQ (https://stedolan.github.io/jq/)
- Fish Shell (https://fishshell.com/)