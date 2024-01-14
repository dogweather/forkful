---
title:                "Bash: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Varför

Att arbeta med JSON, eller JavaScript Object Notation, är en vanlig uppgift för programmerare som arbetar med Bash. JSON är ett strukturerat format för datautbyte, vilket betyder att det är lätt att hantera och läsa av både för människor och datorer. Det är också det format som används för att kommunicera med många API:er och webbtjänster, vilket gör det till en viktig grundläggande kunskap för många projekt.

## Hur man gör

För att börja arbeta med JSON i Bash behöver du först veta hur man läser och manipulerar data. Detta kan uppnås genom att använda kommandot `jq`, som specifikt är utformat för att hantera JSON-data.

```Bash
# Läsa data från en JSON-fil
jq '.' data.json

# Läsa specifika fält från JSON-fil och spara som en variabel
field=$(jq '.field' data.json)

# Lägga till data till en befintlig JSON-fil
jq '. + {"new_field": "new_value"}' data.json

# Utskrift av resultatet
echo $field
```

Output:
```Bash
{
  "field": "value"
}

value
{
  "field": "value",
  "new_field": "new_value"
}
```

## Djupdykning

För att arbeta med JSON på en mer avancerad nivå kan du också lära dig om filter och selektorer. Dessa används för att välja och bearbeta delar av en JSON-struktur.

```Bash
# Välja specifikt fält
jq '.field' data.json

# Välja flera fält
jq '.field1, .field2' data.json

# Använda filter
jq '.field | select(.value > 5)' data.json
```

Output:
```Bash
"value"

{
  "field": "value1",
  "field2": "value2"
}

{
  "value": 10
}
```

Att också förstå syntaxen för JSON är viktigt för att kunna navigera och manipulera data korrekt. JSON består av nycklar och värden, där nyckeln är en sträng som identifierar värdet. Värden kan vara av olika typer, som strängar, nummer, objekt eller listor.

## Se också

* [jq manual](https://stedolan.github.io/jq/manual/)
* [JSON tutorial](https://www.w3schools.com/js/js_json_intro.asp)
* [JSON i bash: Hantera data från API:er](https://www.howtogeek.com/531932/how-to-work-with-json-in-bash-using-jq/)
* [Bash scripting tutorial](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)