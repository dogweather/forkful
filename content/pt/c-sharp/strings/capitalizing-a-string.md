---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:18.501914-07:00
description: "Como fazer: C# oferece uma abordagem direta para capitalizar strings\
  \ usando m\xE9todos integrados. A maneira mais simples de alcan\xE7ar isso \xE9\
  \ modificando a\u2026"
lastmod: '2024-03-13T22:44:46.567590-06:00'
model: gpt-4-0125-preview
summary: "C# oferece uma abordagem direta para capitalizar strings usando m\xE9todos\
  \ integrados."
title: Capitalizando uma string
weight: 2
---

## Como fazer:
C# oferece uma abordagem direta para capitalizar strings usando métodos integrados. A maneira mais simples de alcançar isso é modificando a string diretamente com esses métodos. Para regras de capitalização mais complexas ou específicas (por exemplo, capitalizar cada palavra), bibliotecas adicionais ou métodos manuais podem ser necessários. Abaixo estão exemplos demonstrando como capitalizar uma string de várias maneiras em C#.

### Capitalização Básica:
Para capitalizar a primeira letra de uma única palavra ou frase:

```csharp
string originalString = "hello world";
string capitalizedString = char.ToUpper(originalString[0]) + originalString.Substring(1);
Console.WriteLine(capitalizedString); // Saída: "Hello world"
```

### Capitalizando Cada Palavra:
Para capitalizar a primeira letra de cada palavra em uma string, você pode usar o método `TextInfo.ToTitleCase` encontrado no namespace `System.Globalization`:

```csharp
using System;
using System.Globalization;

string originalString = "hello world";
TextInfo textInfo = CultureInfo.CurrentCulture.TextInfo;
string capitalizedString = textInfo.ToTitleCase(originalString);
Console.WriteLine(capitalizedString); // Saída: "Hello World"
```

Nota: `ToTitleCase` não converte para minúsculas o restante das letras; ele apenas muda para maiúscula a primeira letra de cada palavra. Além disso, certas palavras nas regras de título (como "and", "or", "of") podem não ser capitalizadas dependendo das configurações de cultura.

### Usando Métodos de Extensão para Reutilização:
Você pode criar um método de extensão para a classe `string` para simplificar o processo de capitalização, tornando seu código mais limpo e reutilizável. Aqui está como criar e usar tal método:

```csharp
using System;

public static class StringExtensions
{
    public static string Capitalize(this string input)
    {
        if (string.IsNullOrEmpty(input))
        {
            return input;
        }
        return char.ToUpper(input[0]) + input.Substring(1);
    }
}

class Program
{
    static void Main(string[] args)
    {
        string originalString = "hello world";
        string capitalizedString = originalString.Capitalize();
        Console.WriteLine(capitalizedString); // Saída: "Hello world"
    }
}
```

Este método de extensão `Capitalize` pode ser chamado em qualquer objeto de string dentro do namespace, oferecendo uma abordagem mais intuitiva e orientada a objetos para a manipulação de strings em C#.

### Bibliotecas de Terceiros:
Embora a biblioteca padrão do C# cubra a maioria das necessidades para capitalização de strings, tarefas especializadas específicas podem se beneficiar de bibliotecas de terceiros, como Humanizer. No entanto, para a tarefa de simplesmente capitalizar strings ou cada palavra em uma string, os métodos padrão do C# são adequados e eficientes, negando a necessidade de dependências externas.
