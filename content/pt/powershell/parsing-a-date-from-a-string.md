---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O que é e por que os programadores fazem isso?

Parsear uma data de uma string significa converter uma data armazenada como texto em um formato de data que pode ser facilmente manipulada e utilizada em programas. Programadores fazem isso para poderem trabalhar com datas de forma mais eficiente e precisa em seus códigos, economizando tempo e minimizando erros.

## Como fazer:

```PowerShell
# Exemplo 1: Parsear data de uma string usando o comando Get-Date
Get-Date "28/03/2020"
```
Output: Março 28, 2020

```PowerShell
# Exemplo 2: Parsear data de uma string com formato personalizado
[datetime]::ParseExact("2020-03-28", "yyyy-MM-dd", $null)
```
Output: 28 de março de 2020 00:00:00

## Mergulho profundo:

Parsear datas de strings é uma tarefa comum na programação, especialmente em linguagens que não suportam nativamente o tipo de dado "data". Em PowerShell, o comando Get-Date é uma maneira simples e direta de parsear datas de strings. No entanto, existem outras opções, como a classe [datetime]::ParseExact(), que permite ao usuário especificar um formato personalizado para a data. É importante lembrar que a formatação da data pode variar dependendo do local e da cultura. Por exemplo, a data "02/08/2020" pode ser interpretada como 2 de agosto ou 8 de fevereiro, dependendo do formato de data utilizado.

## Veja também:

- [Documentação do comando Get-Date no Microsoft Docs](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7)
- [Artigo sobre Parseando datas em PowerShell](https://4sysops.com/archives/parsing-date-strings-in-powershell/)
- [Módulo do PowerShell para formatação e conversão de datas](https://github.com/ramblingcookiemonster/PSSQLite/blob/master/SQLite.psm1)