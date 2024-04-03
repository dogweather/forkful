---
date: 2024-01-26 04:44:20.641419-07:00
description: "N\xFAmeros complexos, aqueles com uma parte real e uma parte imagin\xE1\
  ria (como 3 + 4i), s\xE3o vitais em campos como engenharia, f\xEDsica e ci\xEAncia\
  \ de dados.\u2026"
lastmod: '2024-03-13T22:44:46.789192-06:00'
model: gpt-4-0125-preview
summary: "N\xFAmeros complexos, aqueles com uma parte real e uma parte imagin\xE1\
  ria (como 3 + 4i), s\xE3o vitais em campos como engenharia, f\xEDsica e ci\xEAncia\
  \ de dados."
title: "Trabalhando com n\xFAmeros complexos"
weight: 14
---

## O Que & Por Que?
Números complexos, aqueles com uma parte real e uma parte imaginária (como 3 + 4i), são vitais em campos como engenharia, física e ciência de dados. Programadores os usam para simulações, processamento de sinais e resolução de tipos específicos de problemas matemáticos.

## Como Fazer:
O PowerShell não tem suporte interno para números complexos, então você deve criar sua própria solução ou usar o `System.Numerics.Complex` do .NET.

```PowerShell
# Vamos criar números complexos usando .NET
[Reflection.Assembly]::LoadWithPartialName("System.Numerics") | Out-Null

# Criar números complexos
$complex1 = [System.Numerics.Complex]::new(3, 4) # 3 + 4i
$complex2 = [System.Numerics.Complex]::new(1, 2) # 1 + 2i

# Adicionar dois números complexos
$soma = [System.Numerics.Complex]::Add($complex1, $complex2) # 4 + 6i

# Multiplicar dois números complexos
$produto = [System.Numerics.Complex]::Multiply($complex1, $complex2) # -5 + 10i

# Exibir os resultados
"Soma: $soma"
"Produto: $produto"
```
Saída:
```
Soma: (4, 6)
Produto: (-5, 10)
```

## Aprofundando
Números complexos foram desenvolvidos no século 16 para resolver equações que não tinham soluções no reino dos números reais. Agora, são uma pedra angular da matemática moderna.

A dependência do PowerShell no .NET para suporte a números complexos significa que o desempenho é sólido. Alternativas incluem bibliotecas de terceiros ou outras linguagens de programação como Python, onde números complexos são um tipo de dado nativo.

## Veja Também
- [Estrutura System.Numerics.Complex](https://docs.microsoft.com/en-us/dotnet/api/system.numerics.complex)
- [Aritmética de Números Complexos em Python](https://docs.python.org/3/library/cmath.html)
