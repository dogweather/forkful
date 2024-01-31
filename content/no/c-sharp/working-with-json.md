---
title:                "Arbeid med JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"

category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON står for JavaScript Object Notation, og er en lettvekts datautvekslingsformat. Programmerere bruker JSON fordi det er enkelt å lese og skrive, og det er lett å parse og generere i de fleste programmeringsspråk, inkludert C#.

## How to:
La oss se på litt C#-kode for å jobbe med JSON. Vi bruker `System.Text.Json` som er en del av .NET:

```C#
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        // Seriellere et objekt til JSON-string
        var product = new { Name = "Pult", Price = 399.99 };
        string jsonString = JsonSerializer.Serialize(product);
        Console.WriteLine(jsonString);
        
        // Parse JSON-string til et dynamisk objekt
        var parsedProduct = JsonSerializer.Deserialize<dynamic>(jsonString);
        Console.WriteLine($"Produkt: {parsedProduct.Name}, Pris: {parsedProduct.Price}");
    }
}
```

Sample output:
```
{"Name":"Pult","Price":399.99}
Produkt: Pult, Pris: 399.99
```

## Deep Dive
JSON ble introdusert i 2001 og har siden blitt standarden for APIer og webtjenester. Alternativer som XML er mer verbose og mindre populære i dag. Når du jobber med JSON i C#, kan du velge forskjellige biblioteker som `System.Text.Json` eller `Newtonsoft.Json`; sistnevnte gir flere funksjoner, men `System.Text.Json` har bedre ytelse.

## See Also
Her er noen nyttige lenker for videre lesning:

- .NET's offisielle guide til JSON: [Microsoft Docs - JSON in .NET](https://docs.microsoft.com/en-us/dotnet/standard/serialization/system-text-json-overview)
- Newtonsoft.Json (Json.NET): [Newtonsoft.Json Documentation](https://www.newtonsoft.com/json)
- JSON standarden: [Introducing JSON](https://www.json.org/json-en.html)
