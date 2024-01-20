---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Gerar números aleatórios com C#

## O quê e porquê?

Gerar números aleatórios é o processo de criar números que não têm qualquer padrão previsível. Programadores fazem isso por razões como tornar os jogos mais desafiantes ou criar dados de teste.

## Como fazer:

Para gerar números aleatórios em C#, usamos a classe `Random`. Veja um exemplo rápido:

```C#
using System;

class Program {
    static void Main() {
        Random aleatorio = new Random();
        int numero = aleatorio.Next(1, 100);
        Console.WriteLine("Número aleatório: " + numero);
    }
}
```

Neste exemplo, o programa irá gerar e exibir no console um número aleatório entre 1 e 99.

## Mergulhando fundo 

Na origem, a geração de números aleatórios vinha de processos físicos, como o lançamento de dados. No entanto, em programação, esses números são muitas vezes pseudoaleatórios, criados por algoritmos que, embora pareçam aleatórios, podem ser replicados se a "semente" inicial for conhecida.

A alternativa à classe `Random` do C# é usar funções criptográficas para gerar números aleatórios, como a classe `RNGCryptoServiceProvider`. Esta pode ser necessária se precisar de maior segurança e imprevisibilidade.

A função `aleatorio.Next(1, 100)` que usamos anteriormente, utiliza por debaixo dos panos duas funções: `Next()` e `NextDouble()`. A primeira gera números inteiros enquanto a segunda gera números de ponto flutuante.

## Veja também:

- Documentação da Microsoft sobre a classe `Random`: [https://docs.microsoft.com/pt-br/dotnet/api/system.random?view=net-5.0](https://docs.microsoft.com/pt-br/dotnet/api/system.random?view=net-5.0)
- Artigo da Microsoft sobre segurança e aleatoriedade: [https://docs.microsoft.com/pt-br/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-5.0](https://docs.microsoft.com/pt-br/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-5.0)
- Artigo relacionado sobre a geração de números aleatórios em C#: [https://www.c-sharpcorner.com/UploadFile/f0b2ed/random-number-generation-in-C-Sharp/](https://www.c-sharpcorner.com/UploadFile/f0b2ed/random-number-generation-in-C-Sharp/)