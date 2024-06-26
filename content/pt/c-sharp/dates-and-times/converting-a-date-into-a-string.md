---
date: 2024-01-20 17:36:15.208482-07:00
description: "How to: O C# oferece o m\xE9todo `ToString()` para formatar e converter\
  \ datas. Vamos a alguns exemplos."
lastmod: '2024-03-13T22:44:46.595474-06:00'
model: gpt-4-1106-preview
summary: "O C# oferece o m\xE9todo `ToString()` para formatar e converter datas."
title: Convertendo uma data em uma string
weight: 28
---

## How to:
O C# oferece o método `ToString()` para formatar e converter datas. Vamos a alguns exemplos:

```C#
DateTime agora = DateTime.Now;

// Padrão de data e hora
string padrao = agora.ToString();
Console.WriteLine(padrao); // Saída: "04/04/2023 14:23:31"

// Somente data
string soData = agora.ToString("d");
Console.WriteLine(soData); // Saída: "04/04/2023"

// Formato personalizado
string formatoPersonalizado = agora.ToString("dd-MM-yyyy HH:mm");
Console.WriteLine(formatoPersonalizado); // Saída: "04-04-2023 14:23"
```

## Deep Dive
A conversão de datas em strings remonta aos primeiros dias da programação. No contexto de C#, a classe `DateTime` foi introduzida em .NET Framework 1.0 para representar instantes de tempo.

Alternativamente, podemos usar a classe `String.Format` ou interpolação de string para maior legibilidade:

```C#
DateTime agora = DateTime.Now;
string formatado = String.Format("A data e hora atual é: {0:dd/MM/yyyy HH:mm}", agora);
Console.WriteLine(formatado);
// Saída: A data e hora atual é: 04/04/2023 14:23
```

ou 

```C#
DateTime agora = DateTime.Now;
string interpolar = $"Hoje é {agora:dd/MM/yyyy}";
Console.WriteLine(interpolar);
// Saída: Hoje é 04/04/2023
```

No que diz respeito à implementação, o método `ToString` pode ser sobrecarregado para aceitar um `format` e opcionalmente um `IFormatProvider`, como `CultureInfo`, para lidar com formatação sensível ao idioma.

## See Also
- Documentação oficial sobre `DateTime` no MSDN: [DateTime Struct](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netframework-4.8)
- Documentação oficial sobre formatos de data e hora padrão: [Standard Date and Time Format Strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings)
- Documentação oficial sobre formatos de data e hora personalizados: [Custom Date and Time Format Strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
