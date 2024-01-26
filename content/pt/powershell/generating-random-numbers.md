---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:49:37.151286-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
Gerar números aleatórios é como jogar dados digitais. Programadores fazem isso para tudo, desde jogos até simulações e segurança, onde o acaso pode ser uma ferramenta poderosa.

## Como Fazer:
Para gerar números aleatórios em PowerShell, usamos principalmente o cmdlet `Get-Random`. Vamos ver alguns exemplos.

```PowerShell
# Gerar um número aleatório entre 0 e 100
Get-Random -Minimum 0 -Maximum 101
```

```PowerShell
# Gerar um número aleatório de uma array
$cores = 'vermelho', 'azul', 'verde', 'amarelo'
Get-Random -InputObject $cores
```

```PowerShell
# Gerar 10 números aleatórios entre 20 e 30
1..10 | ForEach-Object { Get-Random -Minimum 20 -Maximum 31 }
```

## Aprofundando:
Gerar números aleatórios não é exatamente uma inovação. Matemáticos e engenheiros têm brincado com isso desde os tempos antigos para estudos de probabilidade. Em computadores, desde os primeiros dias, os números aleatórios são necessários para muitos processos.

Historicamente, havia muitos métodos para alcançar aleatoriedade, como lançar moedas ou rolar dados. Com a computação, começamos a usar funções matemáticas para simular este acaso.

Hoje, temos duas categorias principais: 
- RNGs (Random Number Generators), ou GNA em português, que são geralmente rápidos mas previsíveis após certo ponto - ruins para criptografia.
- CSPRNGs (Cryptographically Secure Pseudo-Random Number Generators), ou GNAcs em português, que são projetados para serem imprevisíveis, usados onde a segurança é crítica.

O `Get-Random` no PowerShell é suficiente para a maioria das tarefas do dia-a-dia, mas se você precisar de segurança, talvez tenha que buscar algo mais robusto, como o namespace `System.Security.Cryptography`.

## Veja Também:
- [Documentação oficial do PowerShell `Get-Random`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random)
- [Microsoft: Guia de Criptografia](https://docs.microsoft.com/en-us/dotnet/standard/security/cryptography-model)
- [Wikipedia: Gerador de números aleatórios](https://pt.wikipedia.org/wiki/Gerador_de_n%C3%BAmeros_aleat%C3%B3rios)
