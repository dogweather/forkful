---
date: 2024-01-27 20:32:40.258171-07:00
description: "Gerar n\xFAmeros aleat\xF3rios em C# envolve a cria\xE7\xE3o de valores\
  \ num\xE9ricos imprevis\xEDveis dentro de um intervalo especificado. Programadores\
  \ usam esses m\xE9todos\u2026"
lastmod: '2024-03-13T22:44:46.579987-06:00'
model: gpt-4-0125-preview
summary: "Gerar n\xFAmeros aleat\xF3rios em C# envolve a cria\xE7\xE3o de valores\
  \ num\xE9ricos imprevis\xEDveis dentro de um intervalo especificado."
title: "Gera\xE7\xE3o de n\xFAmeros aleat\xF3rios"
weight: 12
---

## Como fazer:
A maneira mais comum de gerar números aleatórios em C# é usando a classe `System.Random`. Aqui está um exemplo simples demonstrando seu uso:

```C#
using System;

public class ExemploDeNumeroAleatorio
{
    static void Main(string[] args)
    {
        Random random = new Random();
        int numeroAleatorio = random.Next(1, 100); // Gera um número entre 1 e 99
        Console.WriteLine($"Número aleatório: {numeroAleatorio}");
    }
}
```

Isso produzirá um número aleatório como:

```
Número aleatório: 42
```

Para gerar um número flutuante aleatório entre 0.0 e 1.0, você pode usar o método `NextDouble`:

```C#
double randomDouble = random.NextDouble();
Console.WriteLine($"Número duplo aleatório: {randomDouble}");
```

Se você estiver trabalhando em um aplicativo sensível à segurança que requer aleatoriedade criptográfica, é melhor usar a classe `RNGCryptoServiceProvider` encontrada em `System.Security.Cryptography`:

```C#
using System;
using System.Security.Cryptography;

public class ExemploDeNumeroAleatorioSeguro
{
    static void Main()
    {
        byte[] numeroAleatorio = new byte[4]; // Cria um número aleatório de 4 bytes
        using (RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider())
        {
            rng.GetBytes(numeroAleatorio);
        }
        int valor = BitConverter.ToInt32(numeroAleatorio, 0);
        Console.WriteLine($"Número aleatório criptograficamente seguro: {valor}");
    }
}
```

## Aprofundando
A geração de números aleatórios em C# evoluiu ao longo dos anos. Inicialmente, a classe `System.Random` era a opção principal para gerar números pseudo-aleatórios. É pseudo-aleatório porque, dado um valor de semente específico, produzirá a mesma sequência de números, o que pode ser útil para depuração ou repetição de testes.

Embora suficiente para necessidades básicas, `System.Random` não é seguro para threads e pode produzir resultados previsíveis, o que não é adequado para aplicações dependentes de segurança. Esta limitação levou à introdução do `RNGCryptoServiceProvider` para aleatoriedade criptográfica, que é mais seguro, mas também mais intensivo em recursos.

Uma alternativa no .NET Core e no .NET 5+ é a classe `RandomNumberGenerator` em `System.Security.Cryptography` para gerar números aleatórios de forma segura, que é vista como uma opção mais moderna e fácil de usar em comparação com o `RNGCryptoServiceProvider`.

Cada método de geração de números aleatórios em C# tem seu lugar dependendo dos requisitos da aplicação. Para a maioria das aplicações, `System.Random` é suficiente, mas para aquelas que exigem números aleatórios seguros e imprevisíveis, as classes criptográficas fornecem uma alternativa robusta.
