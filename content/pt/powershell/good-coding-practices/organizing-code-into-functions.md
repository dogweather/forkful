---
date: 2024-01-26 01:11:18.285792-07:00
description: "Organizar c\xF3digo em fun\xE7\xF5es consiste em envolver blocos de\
  \ c\xF3digo que executam tarefas espec\xEDficas e atribuir-lhes um nome. Isso \xE9\
  \ feito para tornar o\u2026"
lastmod: '2024-03-13T22:44:46.802425-06:00'
model: gpt-4-1106-preview
summary: "Organizar c\xF3digo em fun\xE7\xF5es consiste em envolver blocos de c\xF3\
  digo que executam tarefas espec\xEDficas e atribuir-lhes um nome. Isso \xE9 feito\
  \ para tornar o\u2026"
title: "Organizando o c\xF3digo em fun\xE7\xF5es"
weight: 18
---

## O Quê & Por Quê?
Organizar código em funções consiste em envolver blocos de código que executam tarefas específicas e atribuir-lhes um nome. Isso é feito para tornar o código reutilizável, legível e sustentável. Em vez de reescrever o mesmo código, chame uma função. Quer depurar ou atualizar? Ajuste a função sem ter que vasculhar montes de scripts.

## Como fazer:
Vamos escrever uma função para calcular a soma de dois números. Simples, mas ilustra o ponto.

```PowerShell
function Add-Numbers {
    param (
        [int]$PrimeiroNum,
        [int]$SegundoNum
    )
    return $PrimeiroNum + $SegundoNum
}

# Chamar a função com 5 e 10
$soma = Add-Numbers -PrimeiroNum 5 -SegundoNum 10
Write-Output "A soma é $soma"
```

Saída de exemplo:

```
A soma é 15
```

## Mergulho Profundo
Funções em PowerShell, como na maioria das linguagens, não são novidade. Estamos compartimentando código desde os tempos do Fortran. É sobre 'não reinventar a roda'. Alternativas? Claro, scripts ou cmdlets. Mas eles carecem da organização e sensibilidade ao contexto das funções dentro dos scripts.

Implementação? As funções podem ser básicas como o nosso exemplo ou complexas, com escopos, entrada de pipeline e mais. Pegue as `Funções Avançadas`. Elas imitam cmdlets com parâmetros que possuem atributos, como `[Parameter(Mandatory=$true)]`. Isso é um vislumbre da flexibilidade do PowerShell.

## Veja Também
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/pt-br/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [about_Script_Blocks](https://docs.microsoft.com/pt-br/powershell/module/microsoft.powershell.core/about/about_script_blocks?view=powershell-7.1)
