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

## O Que é & Por Quê?

Analisar uma data de uma string em PowerShell significa converter a string em um objeto DateTime. Fazemos isso para manipular datas e horários de maneira eficiente em nossos scripts.

## Como Fazer:

Aqui está um exemplo básico de como analisar uma data de uma string.

```PowerShell
$strData = '03/14/2022'
$data = [DateTime]::Parse($strData)
Write-Host $data
```

Essa script tomará a string '03/14/2022' e a converterá em um objeto DateTime. O resultado deste script será '14 de março de 2022 00:00:00'.

## Aprofundando

Historicamente, a análise de dados era uma parte importante do trabalho de um programador, pois os dados eram frequentemente armazenados como strings. Embora existam outras maneiras de alcançar o mesmo resultado (como com o método ParseExact ou TryParse), a abordagem padrão Parse é mais flexível e geralmente será suficiente.

A análise de datas de strings em PowerShell faz uso de classes .NET, especificamente a classe System.DateTime. Isso faz sentido, já que o PowerShell foi construído sobre o .NET Framework.

Por favor, note que ao analisar uma data de uma string, o PowerShell assume que o formato da data corresponde ao formato de data do sistema local. Se isso não for o caso, é possível que você encontre erros. Portanto, se estiver trabalhando com formatos de data que podem não corresponder ao do sistema local, seria melhor usar o método ParseExact.

## Veja Também:

- [Métodos de DateTime no site do .NET](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)