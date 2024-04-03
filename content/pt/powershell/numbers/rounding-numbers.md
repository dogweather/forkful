---
date: 2024-01-26 03:46:17.864731-07:00
description: "Arredondar n\xFAmeros consiste em ajustar um valor para o inteiro mais\
  \ pr\xF3ximo ou para um ponto decimal especificado. Programadores arredondam n\xFA\
  meros para\u2026"
lastmod: '2024-03-13T22:44:46.790126-06:00'
model: gpt-4-0125-preview
summary: "Arredondar n\xFAmeros consiste em ajustar um valor para o inteiro mais pr\xF3\
  ximo ou para um ponto decimal especificado."
title: "Arredondamento de n\xFAmeros"
weight: 13
---

## Como fazer:
Você tem à disposição alguns cmdlets e métodos úteis no PowerShell para arredondamento:

- Método `Round()` da classe Math
```PowerShell
[Math]::Round(15.68) # Arredonda para 16
```
- Especificar decimais:
```PowerShell
[Math]::Round(15.684, 2) # Arredonda para 15.68
```
- `Ceiling()` e `Floor()`, para arredondar sempre para cima ou para baixo:
```PowerShell
[Math]::Ceiling(15.2) # Arredonda para cima para 16
[Math]::Floor(15.9) # Arredonda para baixo para 15
```

## Aprofundamento
Arredondar números não é algo novo; existe desde os tempos antigos, útil para comércio, ciência e controle do tempo. Falando sobre o PowerShell, `[Math]::Round()` segue o "Arredondamento do Banqueiro" por padrão, onde os 0.5 vão para o número par mais próximo, reduzindo o viés em operações estatísticas.

Você não está limitado apenas aos métodos `[Math]` porém. Quer mais controle? Confira `[System.Math]::Round(Number, Digits, MidpointRounding)` onde você pode definir como os pontos médios são tratados: para longe de zero ou para o par (também conhecido como Arredondamento do Banqueiro).

Outro aspecto: o objeto `System.Globalization.CultureInfo`. Ele ajuda com a formatação específica de localidade e preferências de arredondamento ao lidar com números internacionais.

## Veja Também
- Documentação oficial da Microsoft sobre métodos Math: [Link](https://learn.microsoft.com/pt-br/dotnet/api/system.math?view=net-7.0)
- Especificidades do arredondamento decimal no .NET: [Link](https://learn.microsoft.com/pt-br/dotnet/api/system.midpointrounding?view=net-7.0)
- Discussões sobre arredondamento no StackOverflow: [Link](https://stackoverflow.com/questions/tagged/rounding+powershell)
