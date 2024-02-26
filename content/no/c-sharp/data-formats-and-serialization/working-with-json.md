---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:11.840105-07:00
description: "Arbeid med JSON (JavaScript Object Notation) involverer parsing, generering\
  \ og sp\xF8rring av JSON-data, noe som gj\xF8r det til en kritisk ferdighet for\u2026"
lastmod: '2024-02-25T18:49:38.992434-07:00'
model: gpt-4-0125-preview
summary: "Arbeid med JSON (JavaScript Object Notation) involverer parsing, generering\
  \ og sp\xF8rring av JSON-data, noe som gj\xF8r det til en kritisk ferdighet for\u2026"
title: Arbeider med JSON
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Arbeid med JSON (JavaScript Object Notation) involverer parsing, generering og spørring av JSON-data, noe som gjør det til en kritisk ferdighet for moderne programmering. Dette datautvekslingsformatet blir i stadig større grad brukt i webtjenester og APIer på grunn av dets lettlesthet og språkuavhengighet, noe som gjør det essensielt for C#-programmerere som jobber med nettverksapplikasjoner eller interagerer med webbaserte data.

## Hvordan:

### Parse JSON-streng til et objekt

C# tilbyr navneområdet `System.Text.Json` for effektiv JSON-behandling. For å parse en JSON-streng til et C#-objekt, definerer du en klasse som matcher JSON-strukturen og bruker metoden `JsonSerializer.Deserialize`.

```csharp
using System;
using System.Text.Json;

public class Person
{
    public string Navn { get; set; }
    public int Alder { get; set; }
}

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Navn\":\"John\", \"Alder\":30}";
        Person person = JsonSerializer.Deserialize<Person>(jsonString);

        Console.WriteLine($"Navn: {person.Navn}, Alder: {person.Alder}");
        // Utdata: Navn: John, Alder: 30
    }
}
```

### Generere JSON fra et objekt

For å konvertere et C#-objekt tilbake til en JSON-streng, bruk metoden `JsonSerializer.Serialize`.

```csharp
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Navn = "Jane",
            Alder = 25
        };

        string jsonString = JsonSerializer.Serialize(person);
        Console.WriteLine(jsonString);
        // Utdata: {"Navn":"Jane","Alder":25}
    }
}
```

### Bruke Newtonsoft.Json

`Newtonsoft.Json` (eller Json.NET) er et populært tredjepartsbibliotek som tilbyr mer fleksibilitet og alternativer for JSON-serialisering og -deserialisering.

For å bruke Json.NET, må du først installere `Newtonsoft.Json`-pakken via NuGet. Deretter kan du deserialisere en JSON-streng slik:

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Navn\":\"Mike\", \"Alder\":22}";
        Person person = JsonConvert.DeserializeObject<Person>(jsonString);

        Console.WriteLine($"Navn: {person.Navn}, Alder: {person.Alder}");
        // Utdata: Navn: Mike, Alder: 22
    }
}
```

For å generere JSON fra et objekt med Json.NET:

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Navn = "Ella",
            Alder = 28
        };

        string jsonString = JsonConvert.SerializeObject(person);
        Console.WriteLine(jsonString);
        // Utdata: {"Navn":"Ella","Alder":28}
    }
}
```

Disse kodestykkene gir en rask start på håndtering av JSON i C#, og demonstrerer både de innebygde `System.Text.Json`-mulighetene og de omfattende funksjonene til `Newtonsoft.Json`.
