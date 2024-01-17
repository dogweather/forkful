---
title:                "Gerando números aleatórios"
html_title:           "PowerShell: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Gerar números aleatórios é uma técnica utilizada por programadores para produzir valores imprevisíveis de forma automatizada. Isso pode ser útil para simulações, criptografia, jogos e muitas outras aplicações que requerem elementos aleatórios.

## Como fazer:

Para gerar números aleatórios no PowerShell, podemos utilizar o comando Get-Random. Veja abaixo um exemplo simples de como gerar um número aleatório de 1 a 10:

```PowerShell
Get-Random -Minimum 1 -Maximum 10
```

O resultado pode ser qualquer número inteiro entre 1 e 10, como por exemplo 7. Podemos também gerar uma lista de números aleatórios com o parâmetro -Count, que especifica quantos números devem ser gerados. Por exemplo:

```PowerShell
Get-Random -Minimum 1 -Maximum 10 -Count 5
```

Isso irá gerar uma lista de 5 números aleatórios de 1 a 10, tal como: 2, 6, 9, 3 e 1.

## Mergulho Profundo:

Gerar números aleatórios é uma tarefa desafiadora que tem sido estudada por séculos. No passado, dados físicos como moedas, dados e cartas eram usados para simular aleatoriedade. Hoje, com o avanço da tecnologia, é possível gerar números aleatórios sem precisar de nenhum elemento físico. Existem diversas técnicas e algoritmos para gerar números aleatórios, cada um com suas particularidades e níveis de segurança.

Escolher um bom algoritmo para gerar números aleatórios é importante para garantir que a sequência gerada seja o mais próximo possível de uma sequência verdadeiramente aleatória. Além do comando Get-Random, existem outras opções para gerar números aleatórios no PowerShell, como a classe `System.Random` do .NET Framework.

## Veja também:

- [Documentação do Get-Random no Microsoft Docs](https://docs.microsoft.com/pt-br/powershell/module/microsoft.powershell.core/get-random)
- [Artigo sobre geração de números aleatórios no Wikipedia](https://pt.wikipedia.org/wiki/Gera%C3%A7%C3%A3o_de_n%C3%BAmeros_aleat%C3%B3rios)
- [Tutorial sobre como escolher um bom algoritmo para gerar números aleatórios](https://www.random.org/randomness)