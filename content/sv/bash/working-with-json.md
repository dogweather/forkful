---
title:                "Att arbeta med json"
html_title:           "Bash: Att arbeta med json"
simple_title:         "Att arbeta med json"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Varför
Är du intresserad av webbutveckling eller automatisering med Bash? Då kan det vara användbart att lära sig hur man arbetar med JSON-filer!

## Hur man gör
Först och främst behöver du ha Bash installerat på din dator. Sedan är det bara att följa dessa enkla steg:

1. Ladda ner eller skapa en JSON-fil
2. Öppna din terminal och navigera till platsen där filen finns
3. Använd kommandot `cat` för att visa innehållet i filen eller `jq` för en mer läsbar formatering
4. För att extrahera specifika värden från JSON-filen kan du använda kommandot `grep` och `cut` tillsammans med `jq`
5. Du kan också skapa en ny JSON-fil med Bash genom att använda kommandot `echo` och pipa det vidare till en fil med `>` eller `>>`

Här är ett exempel på hur du kan använda kommandon tillsammans för att söka efter alla personer i en JSON-fil vars ålder är över 30 år:

```Bash
cat fil.json | jq '.personer[] | select (.ålder >= 30)' | grep "personer"
```

Output:
```
{
"namn": "Jane",
"ålder": 35
},
{
"namn": "John",
"ålder": 40
}
```

## Deep Dive
JSON-filer är ett vanligt sätt att lagra och överföra data via webben. De är läsbara för både människor och datorer och är enklare att hantera än andra datalösningsformat som XML. Till skillnad från andra format stöder Bash inte inbyggda funktioner för att hantera JSON, vilket betyder att man måste använda verktyg som `jq` för att söka och manipulera data. Det är också viktigt att ha en god förståelse för syntaxen i JSON-filer för att kunna arbeta effektivt.

## Se även
- [Bash-dokumentation](https://www.gnu.org/software/bash/manual/bash.html)
- [jq-dokumentation](https://stedolan.github.io/jq/manual/)
- [JSON-introduktion](https://www.json.org/json-sv.html)