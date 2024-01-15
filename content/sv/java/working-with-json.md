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

## Varför

Om du är intresserad av att hantera data i ett format som är lättläst för både människor och maskiner, kan JSON vara en användbar lösning för dig. Det är ett flexibelt och lättanvänt sätt att lagra och överföra strukturerad data.

## Så här gör du

För att arbeta med JSON i Java, behöver du först importera biblioteket "org.json". Sedan kan du använda metoder som "put" och "get" för att lägga till och hämta data från JSON-objektet.

```Java
// Skapa ett JSON-objekt
JSONObject json = new JSONObject();

// Lägg till data i objektet
json.put("namn", "Anna");
json.put("ålder", 25);

// Hämta data från objektet
String namn = json.getString("namn"); // namn = "Anna"
int ålder = json.getInt("ålder"); // ålder = 25

// Skriv ut JSON-objektet
System.out.println(json.toString());
```

Detta kommer att producera följande utmatning:

```json
{
  "namn": "Anna",
  "ålder": 25
}
```

I Java kan du också läsa in en JSON-fil och konvertera den till ett objekt med hjälp av "JSONObject" klassen.

## Djupdykning

Utöver att lägga till och hämta data, kan du också manipulera och bearbeta JSON-objekt på olika sätt. Till exempel kan du iterera igenom en lista av objekt och utföra åtgärder på varje objekt med hjälp av en "foreach" loop.

```Java
JSONArray lista = new JSONArray();
lista.put(new JSONObject("{\"namn\":\"Anna\",\"ålder\":25}"));
lista.put(new JSONObject("{\"namn\":\"Bob\",\"ålder\":30}"));

// Iterera igenom listan och skriv ut namnen
for (int i = 0; i < lista.length(); i++) {
    JSONObject objekt = lista.getJSONObject(i);
    System.out.println(objekt.getString("namn"));
}
```

Detta kommer att producera följande utmatning:

```Anna
Bob
```

Det är också viktigt att hantera eventuella felaktigheter i JSON-data, till exempel om en icke-existerande nyckel försöks hämtas. Därför är det alltid bra att använda "try-catch" block för att undvika kraschar i ditt program.

## See Also

- [JSON på w3schools](https://www.w3schools.com/js/js_json_intro.asp)
- [Java JSONObject Dokumentation](https://www.json.org/json-en.html)
- [Java try-catch block på Oracle Documentation](https://docs.oracle.com/javase/tutorial/essential/exceptions/try.html)