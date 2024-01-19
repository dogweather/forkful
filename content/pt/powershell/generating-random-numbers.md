---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

A geração de números aleatórios é a criação de números de uma forma que não pode ser razoavelmente prevista. Programadores o fazem para inúmeros propósitos, como jogos, criptografia ou mesmo testes de software.

## Como Fazer:

Aqui estão alguns exemplos de como gerar números aleatórios no PowerShell.

```PowerShell
# gerar um número aleatório entre 0 e a capacidade máxima do int32
$randomNumber = Get-Random
Write-Host "Número aleatório: $randomNumber"

# gerar um número aleatório entre 1 e 100
$randomNumber = Get-Random -Minimum 1 -Maximum 100
Write-Host "Número aleatório entre 1 e 100: $randomNumber"

# gerar um elemento aleatório de um array
$array = 1..10
$randomElement = Get-Random -InputObject $array
Write-Host "Elemento aleatório: $randomElement"
```

## Mergulho Profundo:

O comando Get-Random do PowerShell usa um gerador de números pseudoaleatórios, chamado "System.Random". Ele foi introduzido na versão 2.0 do PowerShell, lançada em 2009.

Existem outras maneiras de gerar números aleatórios, talvez por meio de uma função matemática ou um hardware especializado. Mas o método System.Random é simples, eficiente e bom o suficiente para a maioria das aplicações.

O comando Get-Random gera um número que é determinado por uma semente. Se a semente for a mesma, a sequência de números aleatórios gerados será a mesma. A semente padrão é baseada no tempo do sistema, portanto, na prática, a sequência de números é quase sempre diferente.

## Veja Também:

Aqui estão alguns links para fontes relacionadas:

- [Documentação oficial do comando Get-Random](https://docs.microsoft.com/pt-br/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7.1).
- [Discussão sobre números aleatórios em StackOverflow](https://stackoverflow.com/questions/1646428/what-is-the-best-way-to-generate-random-numbers-in-powershell).
- [Detalhes sobre o método System.Random](https://docs.microsoft.com/pt-br/dotnet/api/system.random?view=net-5.0).