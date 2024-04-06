---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:46.759690-07:00
description: "Como Fazer: **An\xE1lise B\xE1sica:** Os m\xE9todos `DateTime.Parse`\
  \ e `DateTime.TryParse` s\xE3o as op\xE7\xF5es padr\xE3o para converter uma string\
  \ em um `DateTime`. Aqui\u2026"
lastmod: '2024-04-05T21:53:46.937382-06:00'
model: gpt-4-0125-preview
summary: "**An\xE1lise B\xE1sica:** Os m\xE9todos `DateTime.Parse` e `DateTime.TryParse`\
  \ s\xE3o as op\xE7\xF5es padr\xE3o para converter uma string em um `DateTime`."
title: Analisando uma data a partir de uma string
weight: 30
---

## Como Fazer:
**Análise Básica:**

Os métodos `DateTime.Parse` e `DateTime.TryParse` são as opções padrão para converter uma string em um `DateTime`. Aqui está um exemplo rápido:

```csharp
string dateString = "2023-04-12";
DateTime parsedDate;

if (DateTime.TryParse(dateString, out parsedDate))
{
    Console.WriteLine($"Análise bem-sucedida: {parsedDate}");
}
else
{
    Console.WriteLine("Falha ao analisar.");
}
// Saída: Análise bem-sucedida: 12/04/2023 00:00:00
```

**Especificando uma Cultura:**

Às vezes, é necessário analisar uma string de data que está em um formato de cultura específico. Você pode alcançar isso usando a classe `CultureInfo`:

```csharp
using System.Globalization;

string dateString = "12 avril 2023";
var cultureInfo = new CultureInfo("fr-FR");
DateTime parsedDate = DateTime.Parse(dateString, cultureInfo);

Console.WriteLine(parsedDate);
// Saída: 12/04/2023 00:00:00
```

**Análise Exata com um Formato Específico:**

Para cenários em que as datas vêm em um formato específico que pode não ser padrão, `DateTime.ParseExact` é muito útil:

```csharp
string dateString = "Wednesday, 12 April 2023";
string format = "dddd, d MMMM yyyy";
DateTime parsedDate = DateTime.ParseExact(dateString, format, CultureInfo.InvariantCulture);

Console.WriteLine(parsedDate);
// Saída: 12/04/2023 00:00:00
```

**Usando NodaTime:**

Para uma análise de data e hora ainda mais robusta, considere usar a biblioteca de terceiros popular NodaTime. Ela fornece uma gama mais ampla de capacidades de manipulação de data/hora:

```csharp
using NodaTime;
using NodaTime.Text;

var pattern = LocalDatePattern.CreateWithInvariantCulture("yyyy-MM-dd");
var parseResult = pattern.Parse("2023-04-12");

if (parseResult.Success)
{
    LocalDate localDate = parseResult.Value;
    Console.WriteLine(localDate); // 2023-04-12
}
else
{
    Console.WriteLine("Falha ao analisar.");
}
```

NodaTime oferece suporte extensivo para fusos horários, conceitos de período e duração, e muitos sistemas de calendário diferentes, tornando-o uma escolha poderosa para manipulação complexa de data e hora em aplicações .NET.
