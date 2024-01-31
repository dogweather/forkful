---
title:                "Lavorare con JSON"
date:                  2024-01-19
html_title:           "Arduino: Lavorare con JSON"
simple_title:         "Lavorare con JSON"

category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON è un formato dati leggero usato per lo scambio di dati. I programmatori lo usano perché è facile da leggere e scrivere, e ben supportato dai sistemi moderni.

## How to:
```C#
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        // Crea un oggetto
        var pizza = new { Tipo = "Margherita", Ingredienti = new[] { "Pomodoro", "Mozzarella", "Basilico" } };
        
        // Serializza in JSON
        string jsonString = JsonSerializer.Serialize(pizza);
        Console.WriteLine(jsonString);
        
        // Deserializza da JSON
        var deserializedPizza = JsonSerializer.Deserialize<dynamic>(jsonString);
        Console.WriteLine($"Tipo di Pizza: {deserializedPizza.Tipo}");
    }
}
```
Output:
```
{"Tipo":"Margherita","Ingredienti":["Pomodoro","Mozzarella","Basilico"]}
Tipo di Pizza: Margherita
```

## Deep Dive
JSON sta per JavaScript Object Notation ed è stato introdotto all'inizio degli anni 2000. Alternativa a XML, è più compatto, leggibile e più velocemente processabile. In C#, il namespace `System.Text.Json` fornisce funzionalità per serializzare e deserializzare oggetti. Più vecchio è `Newtonsoft.Json` (Json.NET), ma la libreria standard curata da Microsoft è oramai alla pari.

## See Also
- Documentazione Ufficiale System.Text.Json: [docs.microsoft.com](https://docs.microsoft.com/en-us/dotnet/standard/serialization/system-text-json-overview)
- Articolo introduttivo JSON: [json.org](http://json.org/json-it.html)
- Confronto Json.NET vs System.Text.Json: [devblogs.microsoft.com](https://devblogs.microsoft.com/dotnet/try-the-new-system-text-json-apis/)
