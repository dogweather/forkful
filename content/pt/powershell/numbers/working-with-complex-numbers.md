---
date: 2024-01-26 04:44:20.641419-07:00
description: "Como Fazer: O PowerShell n\xE3o tem suporte interno para n\xFAmeros\
  \ complexos, ent\xE3o voc\xEA deve criar sua pr\xF3pria solu\xE7\xE3o ou usar o\
  \ `System.Numerics.Complex` do\u2026"
lastmod: '2024-03-13T22:44:46.789192-06:00'
model: gpt-4-0125-preview
summary: "O PowerShell n\xE3o tem suporte interno para n\xFAmeros complexos, ent\xE3\
  o voc\xEA deve criar sua pr\xF3pria solu\xE7\xE3o ou usar o `System.Numerics.Complex`\
  \ do .NET."
title: "Trabalhando com n\xFAmeros complexos"
weight: 14
---

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
