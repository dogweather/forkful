---
title:                "Utilizando expressões regulares"
html_title:           "PowerShell: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Expressões regulares são padrões utilizados para realizar buscas e manipulações de texto em documentos ou códigos. Programadores as utilizam para tornar o processo de pesquisa e substituição de texto mais eficiente e preciso.

## Como fazer:

Para utilizar expressões regulares no PowerShell, primeiro você precisa importar o módulo `Microsoft.PowerShell.Utility`. Em seguida, você pode usar o operador `-match` para buscar por padrões e o `-replace` para substituir trechos de texto.

Exemplo:
```PowerShell
# Busca por qualquer palavra que comece com "a" e termine com "o"
"arroz abacaxi banana batom" -match "a.*o"

# Saída:
arroz abacaxi banana batom

# Substitui a palavra "amarelo" por "vermelho"
"O céu é amarelo" -replace "amarelo", "vermelho"

# Saída:
O céu é vermelho
```

## Imersão Profunda:

As primeiras expressões regulares foram utilizadas na década de 1950 por matemáticos e linguistas em análise de linguagem natural. Hoje, elas são amplamente utilizadas em programação com diferentes sintaxes para cada linguagem. No PowerShell, além do operador `-match`, também podemos utilizar o comando `[regex]::Match()` para buscar padrões. Alguns programadores preferem usar o módulo `Regex.Match()` do .NET Framework em vez do operador `-match`.

## Veja também:

- [Documentação oficial do PowerShell sobre expressões regulares](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7)