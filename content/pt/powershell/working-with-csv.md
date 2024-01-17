---
title:                "Lidando com csv"
html_title:           "PowerShell: Lidando com csv"
simple_title:         "Lidando com csv"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Trabalhar com arquivos CSV pode ser uma tarefa comum para muitos programadores. CSV é a sigla para "Comma-Separated Values"(Valores Separados por Vírgulas) e é basicamente um arquivo de texto onde os dados são organizados em colunas separadas por vírgulas. Muitos programadores utilizam o formato CSV para manipular grandes conjuntos de dados, pois ele é fácil de ler e escrever por máquinas e também pode ser importado/exportado por diferentes programas de software.

## Como fazer:

Para começar a trabalhar com arquivos CSV no PowerShell, a primeira etapa é importar o módulo `Import-CSV` usando o comando `Import-Module`:

```PowerShell
Import-Module -Name Microsoft.PowerShell.Utility
```

Agora, vamos usar o comando `Import-CSV` para importar um arquivo CSV chamado "dados.csv":

```PowerShell
Import-CSV .\dados.csv
```

Isso irá criar um objeto personalizado chamado `$dados` que pode ser manipulado através de diferentes métodos e propriedades. Por exemplo, para visualizar as colunas e valores do arquivo, podemos usar o comando `Get-Member`:

```PowerShell
$dados | Get-Member
```

Para filtrar e selecionar dados específicos do arquivo, podemos usar o comando `Where-Object` seguido de uma expressão if, como no exemplo abaixo que seleciona todos os dados com a coluna "idade" maior que 18:

```PowerShell
$dados | Where-Object { $_.idade -gt 18 }
```

## Mergulho Profundo:

O formato CSV existe desde a década de 1970 e é amplamente utilizado em diferentes áreas, como finanças, marketing, ciência de dados, entre outros. Além disso, existem outros formatos de arquivos para armazenar dados tabulares, como JSON e XML, mas o formato CSV ainda é amplamente utilizado devido à sua simplicidade e facilidade de uso.

Outras linguagens de programação, como Python e R, também fornecem funcionalidades integradas para trabalhar com arquivos CSV, permitindo uma manipulação de dados mais avançada. No entanto, com o PowerShell, é possível trabalhar com outros formatos de arquivos, como JSON, usando alguns módulos adicionais, como o `ConvertFrom-JSON`.

Ao trabalhar com arquivos CSV, é importante ter em mente que o delimitador pode variar em diferentes países. Por exemplo, enquanto nos Estados Unidos é comum utilizar vírgulas para separar os valores, em países como o Brasil é mais comum o uso do ponto e vírgula. Portanto, é importante verificar qual é o delimitador do seu arquivo CSV para evitar problemas de leitura e manipulação de dados.

## Veja também:

Para mais informações sobre o módulo `Import-CSV` e outras funcionalidades relacionadas ao PowerShell, confira a documentação oficial da Microsoft: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/import-csv

Outra fonte útil para aprender sobre o formato CSV e suas especificações é a documentação do RFC 4180: https://tools.ietf.org/html/rfc4180.