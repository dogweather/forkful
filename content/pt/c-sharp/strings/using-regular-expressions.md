---
aliases:
- /pt/c-sharp/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:25.293542-07:00
description: "Express\xF5es regulares (regex) em C# s\xE3o uma ferramenta poderosa\
  \ para correspond\xEAncia de padr\xF5es dentro de strings, permitindo que programadores\
  \ procurem,\u2026"
lastmod: 2024-02-18 23:08:58.147608
model: gpt-4-0125-preview
summary: "Express\xF5es regulares (regex) em C# s\xE3o uma ferramenta poderosa para\
  \ correspond\xEAncia de padr\xF5es dentro de strings, permitindo que programadores\
  \ procurem,\u2026"
title: "Usando express\xF5es regulares"
---

{{< edit_this_page >}}

## O Que & Por Que?
Expressões regulares (regex) em C# são uma ferramenta poderosa para correspondência de padrões dentro de strings, permitindo que programadores procurem, substituam, dividam ou extraiam dados de forma eficiente. Os programadores utilizam regex para tarefas que variam desde validações simples, como a verificação do formato de e-mail, até tarefas complexas de processamento de texto devido à sua flexibilidade e desempenho.

## Como fazer:

### Correspondência de Padrões Simples
Para verificar se uma string contém um padrão específico, você pode usar o método `Regex.IsMatch` do namespace `System.Text.RegularExpressions`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Hello, World!";
        string pattern = "World";
        bool containsPattern = Regex.IsMatch(sampleText, pattern);

        Console.WriteLine(containsPattern);  // Saída: True
    }
}
```

### Extração de Dados
Extrair dados de uma string usando grupos em um regex pode ser feito com o método `Regex.Match`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Date: 2023-04-12";
        string pattern = @"Date: (\d{4})-(\d{2})-(\d{2})";
        Match match = Regex.Match(sampleText, pattern);

        if (match.Success)
        {
            Console.WriteLine($"Ano: {match.Groups[1].Value}");  // Saída: Ano: 2023
            Console.WriteLine($"Mês: {match.Groups[2].Value}");  // Saída: Mês: 04
            Console.WriteLine($"Dia: {match.Groups[3].Value}");  // Saída: Dia: 12
        }
    }
}
```

### Substituindo Texto
O método `Regex.Replace` permite substituir texto em uma string que corresponde a um padrão especificado.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Visit Microsoft!";
        string pattern = "Microsoft";
        string replacement = "Google";

        string result = Regex.Replace(sampleText, pattern, replacement);

        Console.WriteLine(result);  // Saída: Visit Google!
    }
}
```

### Dividindo Strings
Você pode dividir uma string em um array baseado em um padrão regex usando o método `Regex.Split`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "one,two,three,four,five";
        string pattern = ",";

        string[] result = Regex.Split(sampleText, pattern);

        foreach (string item in result)
        {
            Console.WriteLine(item);
        }
        // Saída: 
        // one
        // two
        // three
        // four
        // five
    }
}
```

### Usando Bibliotecas de Terceiros
Enquanto o Framework .NET fornece suporte extensivo para expressões regulares, também existem bibliotecas de terceiros, como `PCRE.NET`, que oferecem expressões regulares compatíveis com Perl (PCRE) em C#. Isso pode ser útil se você precisar de recursos ou sintaxe do motor de regex do Perl que não estão disponíveis na implementação do .NET.

Para usar `PCRE.NET`, você primeiro instalaria seu pacote NuGet e, então, poderia usá-lo de maneira semelhante a como usa as classes regex nativas do .NET.

```csharp
// Exemplo usando PCRE.NET aqui
// Nota: Imagine um exemplo semelhante aos acima, adaptado para mostrar um recurso único do PCRE.NET.
```

Ao integrar bibliotecas de terceiros para expressões regulares, sempre consulte a documentação delas para informações detalhadas de uso e compatibilidade.
