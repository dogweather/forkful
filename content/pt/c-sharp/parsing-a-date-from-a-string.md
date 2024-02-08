---
title:                "Analisando uma data a partir de uma string"
aliases:
- pt/c-sharp/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:13:46.759690-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analisando uma data a partir de uma string"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?
Analisar (parse) uma data de uma string em C# envolve converter representações textuais de datas e horas em um objeto `DateTime`. Isso é essencial para aplicações que precisam manipular, armazenar ou exibir datas e horas em diferentes formatos, como aplicativos de agenda, processadores de log ou qualquer sistema que lida com entrada de data de usuários ou fontes externas.

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
