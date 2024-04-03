---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:24.120150-07:00
description: "JSON (JavaScript Object Notation) -tiedon k\xE4sittelyyn kuuluu JSON-tiedon\
  \ j\xE4sennys, tuottaminen ja kysely, mik\xE4 tekee siit\xE4 kriittisen taidon\u2026"
lastmod: '2024-03-13T22:44:56.595450-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) -tiedon k\xE4sittelyyn kuuluu JSON-tiedon\
  \ j\xE4sennys, tuottaminen ja kysely, mik\xE4 tekee siit\xE4 kriittisen taidon nykyohjelmoinnissa."
title: "Ty\xF6skentely JSON:n kanssa"
weight: 38
---

## Kuinka:


### JSON-merkkijonon jäsennys olioksi
C# tarjoaa `System.Text.Json` nimiavaruuden tehokkaaseen JSON-käsittelyyn. JSON-merkkijonon jäsentämiseksi C#-olioksi määritä luokka, joka vastaa JSON-rakennetta ja käytä `JsonSerializer.Deserialize`-metodia.

```csharp
using System;
using System.Text.Json;

public class Henkilö
{
    public string Nimi { get; set; }
    public int Ikä { get; set; }
}

public class Ohjelma
{
    public static void Main()
    {
        string jsonString = "{\"Nimi\":\"John\", \"Ikä\":30}";
        Henkilö henkilö = JsonSerializer.Deserialize<Henkilö>(jsonString);

        Console.WriteLine($"Nimi: {henkilö.Nimi}, Ikä: {henkilö.Ikä}");
        // Tuloste: Nimi: John, Ikä: 30
    }
}
```

### JSON:n tuottaminen oliosta
Jotta C#-olion voisi muuttaa takaisin JSON-merkkijonoksi, käytä `JsonSerializer.Serialize`-metodia.

```csharp
using System;
using System.Text.Json;

public class Ohjelma
{
    public static void Main()
    {
        Henkilö henkilö = new Henkilö
        {
            Nimi = "Jane",
            Ikä = 25
        };

        string jsonString = JsonSerializer.Serialize(henkilö);
        Console.WriteLine(jsonString);
        // Tuloste: {"Nimi":"Jane","Ikä":25}
    }
}
```

### Newtonsoft.Json käyttäminen
`Newtonsoft.Json` (tai Json.NET) on suosittu kolmannen osapuolen kirjasto, joka tarjoaa enemmän joustavuutta ja vaihtoehtoja JSON:n serialisointiin ja deserialisointiin.

Json.NET:n käyttämiseen ensin täytyy asentaa `Newtonsoft.Json` paketti NuGetin kautta. Sen jälkeen voit deserialisoida JSON-merkkijonon näin:

```csharp
using System;
using Newtonsoft.Json;

public class Ohjelma
{
    public static void Main()
    {
        string jsonString = "{\"Nimi\":\"Mike\", \"Ikä\":22}";
        Henkilö henkilö = JsonConvert.DeserializeObject<Henkilö>(jsonString);

        Console.WriteLine($"Nimi: {henkilö.Nimi}, Ikä: {henkilö.Ikä}");
        // Tuloste: Nimi: Mike, Ikä: 22
    }
}
```

JSON:n tuottaminen oliosta Json.NET:n avulla:

```csharp
using System;
using Newtonsoft.Json;

public class Ohjelma
{
    public static void Main()
    {
        Henkilö henkilö = new Henkilö
        {
            Nimi = "Ella",
            Ikä = 28
        };

        string jsonString = JsonConvert.SerializeObject(henkilö);
        Console.WriteLine(jsonString);
        // Tuloste: {"Nimi":"Ella","Ikä":28}
    }
}
```

Nämä katkelmat tarjoavat nopean aloituksen JSON:in käsittelyyn C#:ssa, esitellen sekä sisäänrakennetut `System.Text.Json`-ominaisuudet että `Newtonsoft.Json`in laajat ominaisuudet.
