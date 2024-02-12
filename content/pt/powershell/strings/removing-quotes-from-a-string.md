---
title:                "Removendo aspas de uma string"
aliases: - /pt/powershell/removing-quotes-from-a-string.md
date:                  2024-01-26T03:41:37.946398-07:00
model:                 gpt-4-0125-preview
simple_title:         "Removendo aspas de uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Remover aspas de uma string no PowerShell elimina as marcas de aspas simples (`'`) ou duplas (`"`) envolvendo seu texto. Programadores frequentemente precisam limpar strings para processamento, comparação ou propósitos de saída, especialmente quando lidam com entrada de usuário ou análise de arquivos.

## Como fazer:
Você pode utilizar o operador `-replace` para retirar aspas de uma string. Veja como:

```PowerShell
# Substituir aspas simples
$stringComAspasSimples = "'Olá, Mundo!'"
$stringLimpo = $stringComAspasSimples -replace "'", ""
Write-Output $stringLimpo  # Saída: Olá, Mundo!

# Substituir aspas duplas
$stringComAspasDuplas = '"Olá, Mundo!"'
$stringLimpo = $stringComAspasDuplas -replace '"', ""
Write-Output $stringLimpo  # Saída: Olá, Mundo!
```

Para ambos os tipos:

```PowerShell
$stringComAspas = '"Oi, lá," ela disse.'
$stringLimpo = $stringComAspas -replace "[\"']", ""  # Note o uso de classe de caracteres regex
Write-Output $stringLimpo  # Saída: Oi, lá, ela disse.
```

A saída de exemplo do console será algo assim:

```
Olá, Mundo!
Olá, Mundo!
Oi, lá, ela disse.
```

## Aprofundamento
Nos velhos tempos, antes do PowerShell ser um vislumbre nos olhos da Microsoft, o processamento de texto no Windows era frequentemente o domínio de scripts em lote que tinham capacidades limitadas. A introdução do PowerShell trouxe consigo poderosos recursos de manipulação de strings que tornaram a scriptagem muito mais robusta.

Existem alternativas ao `-replace`, como usar o método `.Trim()` para remover aspas apenas no início e no final de uma string, mas elas não oferecem o mesmo controle ou suporte a regex.

```PowerShell
# Usando .Trim() para aspas no início e no fim
$stringComAspas = '"Olá, Mundo!"'
$stringLimpo = $stringComAspas.Trim('"')
Write-Output $stringLimpo  # Saída: Olá, Mundo!
```

Note que, `-replace` usa regex por trás dos panos, então, quando você estiver trabalhando com ele, tenha em mente que caracteres especiais precisam ser escapados se você estiver mirando neles. Se você precisar de um controle mais granular sobre a remoção de aspas, mergulhar no regex com `-replace` é o caminho a seguir, oferecendo-lhe imensa flexibilidade.

## Veja Também
- Para mais sobre regex no PowerShell, confira os documentos oficiais: [about_Regular_Expressions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)
- Descubra outros métodos de strings: [Trim(), TrimStart(), TrimEnd()](https://docs.microsoft.com/en-us/dotnet/api/system.string.trim?view=net-6.0)
