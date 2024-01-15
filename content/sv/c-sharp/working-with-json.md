---
title:                "Att arbeta med json"
html_title:           "C#: Att arbeta med json"
simple_title:         "Att arbeta med json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Varför
Det är vanligt att vilja utbyta data mellan program och system. JSON är en vanlig filformat för att lägga strukturerad data i filer. Så här använder du JSON med C#.

## Hur du gör det
Alla versioner av C# kommer med inbyggda verktyg för att arbeta med JSON. Här är några exempel på hur du kan arbeta med JSON-data i C#.

### Skapa en JSON-fil
Om du vill skapa en JSON-fil i C# kan du använda följande kod:
```C#
var data = new { name = "Anna", age = 25 };
var json = Newtonsoft.Json.JsonConvert.SerializeObject(data);
System.IO.File.WriteAllText(@"C:\Users\Anna\Documents\data.json", json);
```
Detta skapar en JSON-fil med innehållet 
```json
{ 
    "name": "Anna", 
    "age": 25 
}
```

### Läsa en JSON-fil
Om du redan har en JSON-fil och vill läsa in datan i din C#-kod kan du använda följande kod:
```C#
var json = System.IO.File.ReadAllText(@"C:\Users\Anna\Documents\data.json");
var data = Newtonsoft.Json.JsonConvert.DeserializeObject(json);
Console.WriteLine(data.name); // Skriver ut "Anna"
```

### Konvertera mellan JSON och C#-objekt
JSON-filer kan enkelt konverteras till C#-objekt och vice versa. I exemplet nedan skapar vi ett C#-objekt från JSON-datan och sedan konverterar tillbaka till en JSON-sträng:
```C#
var json = System.IO.File.ReadAllText(@"C:\Users\Anna\Documents\data.json");
var data = Newtonsoft.Json.JsonConvert.DeserializeObject(json);
Console.WriteLine(data.name); // Skriver ut "Anna"

// Konverterar C#-objektet tillbaka till en JSON-sträng
var newJson = Newtonsoft.Json.JsonConvert.SerializeObject(data);
Console.WriteLine(newJson); // Skriver ut { "name": "Anna", "age": 25 }
```

## Deep Dive
JSON är en lättviktig och läsbar filformat som ofta används för att överföra data mellan olika system. C# har många inbyggda funktioner för att arbeta med JSON, inklusive deserialisering och serialisering av objekt. Det finns också flera externa bibliotek för mer avancerad hantering av JSON-filer.

## Se även
- [Officiell dokumentation för System.Text.Json Namespace](https://docs.microsoft.com/en-us/dotnet/api/system.text.json?view=netcore-3.1)
- [JSON i .NET: En tutorial](https://www.newtonsoft.com/json/help/html/Introduction.htm)
- [Detaljerad information om JSON och C#](https://www.c-sharpcorner.com/UploadFile/dhananjaycoder/json-serialization-and-deserialization-in-C-Sharp/)