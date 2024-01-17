---
title:                "Arbeta med json"
html_title:           "Java: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med JSON är ett sätt för programmerare att hantera data på ett strukturerat sätt. JSON står för JavaScript Object Notation och är ett populärt format för att lagra och överföra data mellan applikationer och system. Genom att använda JSON kan man enkelt hantera och strukturera data på ett sätt som är lättläst både för människor och datorer.

## Så här gör man:
```Java
// För att använda JSON i ditt Java-program, behöver du importera JSON-biblioteket:
import org.json.*;

// Skapa ett JSON-objekt:
JSONObject obj = new JSONObject();

// Lägg till data i objektet:
obj.put("namn", "Anna");
obj.put("ålder", 25);

// Skriv ut JSON-objektet:
System.out.println(obj);
```
Output: {"namn":"Anna","ålder":25}

## Djupdykning:
JSON har funnits sedan 2002 och har blivit ett populärt alternativ till XML för att hantera data. Det är ett enkelt format som är lätt att läsa och skriva för både människor och datorer. Det finns också många bibliotek och verktyg tillgängliga för att arbeta med JSON i olika programmeringsspråk.

## Se även:
- [JSON - officiell hemsida](https://www.json.org/)
- [JSON Tutorial för Java](https://www.tutorialspoint.com/json/json_java_example.htm)
- [Alternativ till JSON](https://www.atmantra.com/machine-learning-101/serialization/json-vs-xml-vs-yaml-a-comparison/) (jämförelse mellan JSON, XML och YAML)