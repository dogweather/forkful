---
title:                "JSON-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Arduino: JSON-tiedostojen käsittely"
simple_title:         "JSON-tiedostojen käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON eli JavaScript Object Notation on tiedonvaihtoformaatti. Helpolla luettavuudella ja keveydellä se on ideaali webb-palvelinten ja -sovellusten väliseen dataan, siksi koodarit käyttävät sitä paljon.

## How to:
```C#
using System;
using System.Text.Json;

class Program
{
    static void Main()
    {
        // JSON-olion luominen
        string json = "{\"nimi\": \"Matti\", \"ika\": 30}";
        
        // Deserialisointi
        Henkilo henkilo = JsonSerializer.Deserialize<Henkilo>(json);
        Console.WriteLine($"{henkilo.Nimi} on {henkilo.Ika} vuotta vanha.");
        
        // Muuttujan muokkaus
        henkilo.Ika = 31;
        
        // Serialisointi
        string uusiJson = JsonSerializer.Serialize(henkilo);
        Console.WriteLine(uusiJson);
    }
}

public class Henkilo
{
    public string Nimi { get; set; }
    public int Ika { get; set; }
}
```
Output:
```
Matti on 30 vuotta vanha.
{"nimi":"Matti","ika":31}
```

## Deep Dive
JSON lanseerattiin 2000-luvun alussa ja se on XML:n kevyempi vaihtoehto. C#:ssa `System.Text.Json` ja `Newtonsoft.Json` ovat suosittuja kirjastoja JSONin käsittelyyn. `System.Text.Json` on .NET Core 3.0:sta lähtien vakio, sen suorituskyky on hyvä ja se tukee uusinta C#-standardia, kun taas `Newtonsoft.Json` tarjoaa lisäominaisuuksia ja laajempaa yhteensopivuutta.

## See Also
- Microsoftin JSON-dokumentaatio: [docs.microsoft.com](https://docs.microsoft.com/en-us/dotnet/standard/serialization/system-text-json-overview)
- Newtonsoft.Json-dokumentaatio: [www.newtonsoft.com](https://www.newtonsoft.com/json)
- JSON-standardin kotisivu: [www.json.org](http://json.org/)
