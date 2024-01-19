---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O que e Por que?
Substituir texto em programação significa trocar uma instância de um caractere ou uma sequência de caracteres por outra em uma string. Fazemos isso para corrigir erros, atualizar informações e aprimorar a usabilidade do aplicativo.

## Como fazer:
Vamos começar com um exemplo simples. Para substituir 'bom dia' por 'boa tarde' em uma string, você pode usar o comando `Replace` assim:

```PowerShell
$string = "Bom dia, amigos!"
$trocar = $string.Replace("bom dia", "boa tarde")
echo $trocar
```

A saída será:

```PowerShell
Boa tarde, amigos!
```

## Mergulhando Fundo
A função `Replace` vem do .NET Framework, onde PowerShell foi inicialmente desenvolvido, e é muito útil para manipular strings.

Alternativamente, você pode usar o cmdlet `Replace` de PowerShell ou usar expressões regulares para substituições mais complexas. No entanto, eles podem ser mais difíceis de implementar que o método `Replace()`. Aqui está um exemplo usando expressão regular:

```PowerShell
$string = "Bom dia, amigos!"
$trocar = $string -replace 'bom dia','boa tarde'
echo $trocar
```

A saída também será:

```PowerShell
Boa tarde, amigos!
```

## Veja Também
Para mais detalhes, você pode consultar os seguintes links:

1. [PowerShell Basics: String manipulation](https://www.randika.info/2020/05/23/powershell-basics-string-manipulation-methods/)
2. [PowerShell Replace “Method” Versus “Operator”](https://adamtheautomator.com/powershell-replace/)
3. [Introduction to Regular Expressions (Regex) in PowerShell](https://adamtheautomator.com/regex-powershell/)