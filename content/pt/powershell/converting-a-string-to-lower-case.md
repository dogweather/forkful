---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O Que & Por Quê? 

Converter uma string para caixa baixa significa transformar todas as letras maiúsculas presentes na string em letras minúsculas. Programadores fazem isso para padronizar a entrada de dados, facilitando comparações e buscas.

## Como Fazer:

PowerShell torna esse processo surpreendentemente simples. Vamos usar o método .ToLower() após uma string. Aqui está um exemplo:

```PowerShell
$Texto = "Olá, Mundo!"
$TextoMinusculo = $Texto.ToLower()
$TextoMinusculo
```

A saída será:

```PowerShell
olá, mundo!
```

Este comando transformara todos os caracteres maiúsculos em minúsculos do texto contido na variável `$Texto`.

## Deep Dive

Historicamente, a normalização de dados, incluindo a conversão para caixa baixa, tem sido fundamental para algoritmos de comparação e pesquisa de strings. No PowerShell, a função ToLower é uma implementação padrão da funcionalidade de conversão de caixa de .NET.

No entanto, é importante conhecer algumas alternativas, assim como entender as nuances da implementação do ToLower. Embora ToLower seja a abordagem mais comum, também pode usar ToLowerInvariant quando for necessário garantir consistência, independentemente das configurações de cultura do sistema.

Agora vamos para os detalhes de implementação. Quando usamos ToLower(), ele retorna uma cópia da string convertida, enquanto a string original não é modificada. Isso ocorre porque as strings em .NET (e, portanto, PowerShell) são imutáveis.

```PowerShell
$Texto = "Olá, Mundo!"
$TextoMinusculo = $Texto.ToLower()
$Texto
$TextoMinusculo
```

A saída será:

```PowerShell
Olá, Mundo!
olá, mundo!
```

## Ver Também

1. Documentação oficial do PowerShell: [aqui](https://docs.microsoft.com/pt-br/powershell/)
2. Método ToLower(): [aqui](https://docs.microsoft.com/pt-br/dotnet/api/system.string.tolower)
3. Método ToLowerInvariant(): [aqui](https://docs.microsoft.com/pt-br/dotnet/api/system.string.tolowerinvariant)
4. Discussão sobre ToLower vs. ToLowerInvariant na StackOverflow: [aqui](https://stackoverflow.com/questions/2801508/lowercasestring-tolower-vs-tolowerinvariant)
5. Mais informações sobre strings imutáveis em C# (aplicável ao PowerShell): [aqui](https://www.c#tutorial.net/csharp-string-immutable/)