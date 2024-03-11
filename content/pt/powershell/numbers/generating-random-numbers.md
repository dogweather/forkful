---
date: 2024-01-27 20:34:56.226763-07:00
description: "Gerar n\xFAmeros aleat\xF3rios no PowerShell \xE9 sobre criar valores\
  \ num\xE9ricos imprevis\xEDveis dentro de uma faixa especificada. Programadores\
  \ utilizam essa\u2026"
lastmod: '2024-03-11T00:14:20.514786-06:00'
model: gpt-4-0125-preview
summary: "Gerar n\xFAmeros aleat\xF3rios no PowerShell \xE9 sobre criar valores num\xE9\
  ricos imprevis\xEDveis dentro de uma faixa especificada. Programadores utilizam\
  \ essa\u2026"
title: "Gera\xE7\xE3o de n\xFAmeros aleat\xF3rios"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Gerar números aleatórios no PowerShell é sobre criar valores numéricos imprevisíveis dentro de uma faixa especificada. Programadores utilizam essa capacidade por uma miríade de razões, incluindo testes, simulação e propósitos de segurança, onde a imprevisibilidade ou a imitação de aleatoriedade do mundo real é crucial.

## Como fazer:
O PowerShell oferece uma abordagem direta para gerar números aleatórios usando o cmdlet `Get-Random`. Este cmdlet pode produzir números aleatórios dentro de uma faixa padrão ou uma faixa especificada.

```PowerShell
# Gerar um número aleatório entre 0 e Int32.MaxValue
$randomNumber = Get-Random
Write-Output $randomNumber
```

Para especificar uma faixa, use os parâmetros `-Minimum` e `-Maximum`:

```PowerShell
# Gerar um número aleatório entre 1 e 100
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```

Para mais controle, você pode instanciar um objeto da classe `System.Random`:

```PowerShell
# Usando System.Random para uma sequência de números
$rand = New-Object System.Random
foreach ($i in 1..5) {
    $randomNumber = $rand.Next(1, 101)
    Write-Output $randomNumber
}
```

Se você precisa de uma seleção aleatória de uma matriz ou coleção, `Get-Random` pode escolher diretamente um item:

```PowerShell
# Seleção aleatória de uma matriz
$array = 1..10
$randomItem = Get-Random -InputObject $array
Write-Output $randomItem
```

## Mergulho Profundo
O cmdlet `Get-Random` no PowerShell utiliza a classe .NET `System.Random` por trás dos panos para gerar números pseudorrandômicos. Eles são "pseudo" porque usam algoritmos para produzir sequências de números que apenas parecem aleatórios. Para a maioria das aplicações, esse nível de aleatoriedade é suficiente. No entanto, para casos de uso que requerem segurança criptográfica, `System.Random` não é adequado devido à sua natureza previsível.

PowerShell e .NET oferecem `System.Security.Cryptography.RNGCryptoServiceProvider` para aleatoriedade criptográfica, que é mais apropriado para gerar chaves de criptografia ou outras operações sensíveis à segurança:

```PowerShell
# Números aleatórios criptograficamente seguros
$rng = [System.Security.Cryptography.RNGCryptoServiceProvider]::new()
$bytes = New-Object byte[] 4
$rng.GetBytes($bytes)
$randomNumber = [BitConverter]::ToInt32($bytes, 0)
Write-Output $randomNumber
```

Enquanto `Get-Random` e `System.Random` satisfazem um amplo conjunto de necessidades para aleatoriedade em scripts e lógica de aplicativos, é essencial selecionar a ferramenta certa para o trabalho, especialmente em aplicações centradas em segurança onde a previsibilidade pode apresentar uma vulnerabilidade.
