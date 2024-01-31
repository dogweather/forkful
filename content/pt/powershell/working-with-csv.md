---
title:                "Trabalhando com CSV"
date:                  2024-01-19
simple_title:         "Trabalhando com CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Trabalhar com CSV (valores separados por vírgula) é lidar com dados tabelados de forma simples e amplamente suportada. Programadores fazem isso para importar, exportar, armazenar e manipular dados facilmente entre sistemas e aplicações.

## Como Fazer:
Para importar dados de um arquivo CSV, use `Import-Csv`. Veja um exemplo:

```PowerShell
$dados = Import-Csv -Path 'caminho/para/o/seuarquivo.csv'
$dados
```

Para criar e escrever dados em um arquivo CSV:

```PowerShell
$objetos = @(
    [PSCustomObject]@{Nome='Ana'; Idade=23},
    [PSCustomObject]@{Nome='João'; Idade=30}
)
$objetos | Export-Csv -Path 'caminho/para/o/novoarquivo.csv' -NoTypeInformation
```

Se precisar adicionar dados sem sobrescrever:

```PowerShell
$novoObjeto = [PSCustomObject]@{Nome='Carlos'; Idade=42}
$novoObjeto | Export-Csv -Path 'caminho/para/o/novoarquivo.csv' -Append -NoTypeInformation
```

## Mergulho Profundo
CSV é um formato antigo, existindo desde antes dos anos 70, e se destaca pela simplicidade. Alternativas incluem JSON ou XML, que suportam estruturas de dados mais complexas. A implementação do trabalho com CSV em PowerShell é direta graças aos comandos `Import-Csv` e `Export-Csv`, que tratam automaticamente da codificação do arquivo e manipulação de dados.

## Veja Também
- [Documentação Oficial do PowerShell](https://docs.microsoft.com/en-us/powershell/)
- [CSV na Wikipedia](https://pt.wikipedia.org/wiki/Comma-separated_values)
- [Import-Csv no PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/import-csv)
- [Export-Csv no PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/export-csv)
