---
title:                "Python: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/working-with-json.md"
---

{{< edit_this_page >}}

## Varför
Berätta kort om varför man skulle använda sig av JSON i sin programmering.

JSON (JavaScript Object Notation) är ett vanligt format för att överföra och lagra data. Det är ett lättviktigt och lättläst sätt att strukturera och dela data, vilket gör det till ett användbart verktyg för att hantera data i dina Python-program.

## Så här gör du
Visa hur man kan arbeta med JSON i Python genom att ge kodexempel och förväntad utmatning.

```Python
# Importera JSON-biblioteket
import json

# Skapa en JSON-sträng
data = '{"namn": "Maria", "ålder": 27, "hobbyer": ["läsa", "måla", "resa"]}'

# Konvertera JSON-strängen till ett Python-dictionary
person = json.loads(data)

# Skriv ut personens namn
print(person["namn"]) # Output: Maria

# Skriv ut personens ålder
print(person["ålder"]) # Output: 27

# Skriv ut personens hobbyer
for hobby in person["hobbyer"]:
  print(hobby) # Output: läsa, måla, resa
```

## Djupdykning
Ge mer detaljerad information om hur man kan arbeta med JSON i Python.

JSON kommer i form av en textsträng och måste därför tolkas av programmet för att kunna användas som data. När vi importerar JSON-biblioteket kan vi använda funktioner som `loads()` för att översätta JSON till ett Python-objekt och `dumps()` för att översätta ett Python-objekt till en JSON-sträng.

När man arbetar med JSON är det viktigt att känna till att det följer en sträng struktur som består av uppsättningar av nycklar och värden. Nycklar är alltid strängar och värden kan vara av olika typer, som strängar, numeriska värden, listor eller dictionary.

## Se även
- [Python JSON-dokumentation](https://docs.python.org/3/library/json.html)
- [JSON officiella hemsida](https://www.json.org/json-sv.html)