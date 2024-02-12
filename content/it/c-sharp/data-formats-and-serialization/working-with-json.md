---
title:                "Lavorare con JSON"
aliases:
- /it/c-sharp/working-with-json.md
date:                  2024-02-03T19:22:12.231814-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Lavorare con JSON (JavaScript Object Notation) implica l'analisi, la generazione e l'interrogazione di dati JSON, rendendolo una competenza fondamentale per la programmazione moderna. Questo formato di scambio dati è estremamente utilizzato nei servizi web e nelle API grazie alla sua facile leggibilità e indipendenza linguistica, rendendolo essenziale per i programmatori C# che lavorano su applicazioni in rete o interagiscono con dati basati sul web.

## Come fare:

### Analisi di una Stringa JSON in un Oggetto

C# fornisce il namespace `System.Text.Json` per un'elaborazione JSON efficiente. Per analizzare una stringa JSON in un oggetto C#, definisci una classe che corrisponda alla struttura JSON e utilizza il metodo `JsonSerializer.Deserialize`.

```csharp
using System;
using System.Text.Json;

public class Person
{
    public string Nome { get; set; }
    public int Eta { get; set; }
}

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Nome\":\"John\", \"Eta\":30}";
        Person persona = JsonSerializer.Deserialize<Person>(jsonString);

        Console.WriteLine($"Nome: {persona.Nome}, Eta: {persona.Eta}");
        // Output: Nome: John, Eta: 30
    }
}
```

### Generazione di una Stringa JSON da un Oggetto

Per convertire un oggetto C# nuovamente in una stringa JSON, usa il metodo `JsonSerializer.Serialize`.

```csharp
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        Person persona = new Person
        {
            Nome = "Jane",
            Eta = 25
        };

        string jsonString = JsonSerializer.Serialize(persona);
        Console.WriteLine(jsonString);
        // Output: {"Nome":"Jane","Eta":25}
    }
}
```

### Usando Newtonsoft.Json

`Newtonsoft.Json` (o Json.NET) è una popolare libreria di terze parti che offre maggiore flessibilità e opzioni per la serializzazione e deserializzazione JSON.

Per usare Json.NET, devi prima installare il pacchetto `Newtonsoft.Json` tramite NuGet. Dopodiché, puoi deserializzare una stringa JSON in questo modo:

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Nome\":\"Mike\", \"Eta\":22}";
        Person persona = JsonConvert.DeserializeObject<Person>(jsonString);

        Console.WriteLine($"Nome: {persona.Nome}, Eta: {persona.Eta}");
        // Output: Nome: Mike, Eta: 22
    }
}
```

Per generare JSON da un oggetto con Json.NET:

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        Person persona = new Person
        {
            Nome = "Ella",
            Eta = 28
        };

        string jsonString = JsonConvert.SerializeObject(persona);
        Console.WriteLine(jsonString);
        // Output: {"Nome":"Ella","Eta":28}
    }
}
```

Questi frammenti offrono un rapido inizio per gestire il JSON in C#, dimostrando sia le capacità incorporate di `System.Text.Json` che le estese funzionalità di `Newtonsoft.Json`.
