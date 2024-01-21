---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:49:02.708742-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Gerar números aleatórios é como rolar um dado eletrônico; nunca sabemos que número vai sair. Programadores usam isso para adicionar aleatoriedade e imprevisibilidade em jogos, simulações e até na segurança de aplicações.

## How to:
No C#, é moleza. Usamos a classe `Random` para criar números aleatórios. Veja só:

```C#
using System;

class Program
{
    static void Main()
    {
        Random rng = new Random();
        int numeroAleatorio = rng.Next(1, 101); // Gera um número entre 1 e 100
        Console.WriteLine($"Número aleatório: {numeroAleatorio}");
    }
}
```

Se rodar esse código, você vai ver uma linha tipo:
```
Número aleatório: 42
```
Toda vez pode ser um número diferente!

## Deep Dive
Lá em 1955, John von Neumann já mandava ver numas paradas chamadas "Middle-square method", mas hoje em dia, isso aí já é história. O `System.Random`, que usamos ali em cima, usa um gerador congruente linear — bom pra uso geral, mas não ideal para criptografia. Para isso, temos o `System.Security.Cryptography.RandomNumberGenerator` no C#, que é bem mais robusto.

Alternativas? Certo, você pode querer mais controle ou precisão. O `System.Random` é legal para coisas simples. Mas se precisar de algo mais pesado em ciência ou segurança, olhe para fora da BCL (Base Class Library). Há bibliotecas como a MathNet.Numerics, que oferecem geração de números aleatórios com distribuições estatísticas variadas.

O `Random` do .NET usa uma semente com base no relógio do sistema por padrão. Isso significa que se você criar muitos objetos `Random` em um curto período de tempo, eles podem gerar a mesma sequência de números. Para contornar isso, use uma semente diferente manualmente ou compartilhe uma única instância de `Random` se segurança não for uma preocupação.

## See Also
- [Documentação oficial Random Class](https://docs.microsoft.com/pt-br/dotnet/api/system.random?view=net-6.0)
- [MathNet.Numerics Documentation](https://numerics.mathdotnet.com/)
- [Microsoft's RNGCryptoServiceProvider Class](https://docs.microsoft.com/pt-br/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-6.0)