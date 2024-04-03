---
date: 2024-01-20 17:45:18.132916-07:00
description: "How to: No C#, voc\xEA pode usar o m\xE9todo `Substring()` da classe\
  \ `String` para pegar partes de uma string. Aqui est\xE3o alguns exemplos."
lastmod: '2024-03-13T22:44:46.573317-06:00'
model: gpt-4-1106-preview
summary: "No C#, voc\xEA pode usar o m\xE9todo `Substring()` da classe `String` para\
  \ pegar partes de uma string."
title: Extraindo substrings
weight: 6
---

## How to:
No C#, você pode usar o método `Substring()` da classe `String` para pegar partes de uma string. Aqui estão alguns exemplos:

```C#
string textoCheio = "Olá, programadores!";
string saudacao = textoCheio.Substring(0, 4); // Pega "Olá,"
string audiencia = textoCheio.Substring(5); // Pega "programadores!"

Console.WriteLine(saudacao); // Saída: Olá,
Console.WriteLine(audiencia); // Saída: programadores!
```

## Deep Dive
O uso de substrings não é novidade na programação. Métodos para extrair substrings existem há décadas, adaptando-se a diferentes linguagens de programação. Em C#, há alternativas ao `Substring()`, como o uso de LINQ para filtrar caracteres ou o método `Split()` para dividir a string em várias partes baseando-se em delimitadores.

A implementação do `Substring()` é otimizada para ser rápida e eficiente, mas ainda assim precisa ser usada com cuidado para evitar erros de 'index out of range'. As versões mais recentes do C# incluem novidades como `Span<T>` e `Memory<T>`, proporcionando formas mais eficientes de manipular strings sem a necessidade de criar novos objetos.

## See Also
- [Microsoft's official documentation for String.Substring Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
- [Example of using Split method on a string](https://docs.microsoft.com/en-us/dotnet/csharp/how-to/parse-strings-using-split)
- [Introduction to LINQ queries](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/)
