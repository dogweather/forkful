---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Conversão de Data para String em PowerShell: O Que, Porquê e Como

## O Que e Porquê?
Conversão de data para string é o processo de transformar um objeto de data em uma string, ou seja, numa sequência de caracteres. Programadores fazem isso para facilitar a leitura e a manipulação dos dados de data.

## Como fazer:
No PowerShell, você pode usar o método `ToString()` para converter uma data em uma string. Aqui tem um exemplo:

```PowerShell
$data = Get-Date
$stringData = $data.ToString('dd/MM/yyyy')
Write-Output $stringData
```
Esse código irá converter a data atual em uma string no formato 'dd/MM/yyyy'. A saída poderia ser algo como: '07/04/2022'.

## Deep Dive
Historicamente, a conversão de datas em strings tem sido uma ferramenta importante para programadores. This is due in part to the fact that different systems may represent dates in different ways.

Existem várias alternativas para converter datas em strings no PowerShell. Uma delas é o formato de sequência de dígitos (`'yyyyMMdd'`), que é útil quando você precisa ordenar datas como strings.

No PowerShell, a conversão de datas em strings é feita através da classe .NET `DateTime`, que fornece o método `ToString()`. Esse método possui muitas sobrecargas que permitem formatar a string de diferentes maneiras.

## Veja Também
Para dives mais profundos no tema de conversão de data para string, consulte os seguintes recursos:

1. [Microsoft Documentation - ToString Method](https://docs.microsoft.com/pt-br/dotnet/api/system.datetime.tostring?view=net-6.0)
2. [PowerShell Basics: Formatting Dates and Times](https://devblogs.microsoft.com/scripting/poweshell-basics-formatting-dates-and-times/)
3. [DateTime Formats in PowerShell](https://www.howtogeek.com/409615/datetime-formats-in-powershell/)