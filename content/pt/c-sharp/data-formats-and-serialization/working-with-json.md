---
title:                "Trabalhando com JSON"
aliases:
- pt/c-sharp/working-with-json.md
date:                  2024-02-03T19:22:10.596502-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

Trabalhar com JSON (JavaScript Object Notation) envolve analisar, gerar e consultar dados JSON, tornando-se uma habilidade crítica para a programação moderna. Esse formato de troca de dados é extremamente usado em serviços web e APIs devido à sua fácil legibilidade e independência de linguagem, o que o torna essencial para programadores C# que trabalham com aplicações em rede ou interagindo com dados baseados na web.

## Como Fazer:

### Analisando Cadeia de Caracteres JSON para um Objeto

C# fornece o namespace `System.Text.Json` para processamento eficiente de JSON. Para analisar uma cadeia de caracteres JSON para um objeto C#, defina uma classe que corresponda à estrutura JSON e use o método `JsonSerializer.Deserialize`.

```csharp
using System;
using System.Text.Json;

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
}

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Name\":\"John\", \"Age\":30}";
        Person person = JsonSerializer.Deserialize<Person>(jsonString);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
        // Saída: Name: John, Age: 30
    }
}
```

### Gerando JSON a partir de um Objeto

Para converter um objeto C# de volta para uma cadeia de caracteres JSON, use o método `JsonSerializer.Serialize`.

```csharp
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Name = "Jane",
            Age = 25
        };

        string jsonString = JsonSerializer.Serialize(person);
        Console.WriteLine(jsonString);
        // Saída: {"Name":"Jane","Age":25}
    }
}
```

### Usando Newtonsoft.Json

`Newtonsoft.Json` (ou Json.NET) é uma biblioteca de terceiros popular que oferece mais flexibilidade e opções para serialização e desserialização JSON.

Para usar Json.NET, você deve primeiro instalar o pacote `Newtonsoft.Json` via NuGet. Então, você pode desserializar uma cadeia de caracteres JSON assim:

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Name\":\"Mike\", \"Age\":22}";
        Person person = JsonConvert.DeserializeObject<Person>(jsonString);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
        // Saída: Name: Mike, Age: 22
    }
}
```

Para gerar JSON a partir de um objeto com Json.NET:

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Name = "Ella",
            Age = 28
        };

        string jsonString = JsonConvert.SerializeObject(person);
        Console.WriteLine(jsonString);
        // Saída: {"Name":"Ella","Age":28}
    }
}
```

Esses trechos oferecem um rápido início para o manuseio de JSON em C#, demonstrando tanto as capacidades incorporadas do `System.Text.Json` quanto os recursos extensivos do `Newtonsoft.Json`.
