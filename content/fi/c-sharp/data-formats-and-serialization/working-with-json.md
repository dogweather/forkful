---
title:                "Työskentely JSON:n kanssa"
date:                  2024-02-03T19:22:24.120150-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely JSON:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

JSON (JavaScript Object Notation) -tiedon käsittelyyn kuuluu JSON-tiedon jäsennys, tuottaminen ja kysely, mikä tekee siitä kriittisen taidon nykyohjelmoinnissa. Tämä datanvaihtoformaatti on erittäin käytetty web-palveluissa ja API:issa sen helpon luettavuuden ja kieliriippumattomuuden vuoksi, mikä tekee siitä olennaisen taidon C#-ohjelmoijille, jotka työskentelevät verkottuneissa sovelluksissa tai vuorovaikuttavat web-pohjaisen datan kanssa.

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
