---
title:                "C#: Gerando números aleatórios"
programming_language: "C#"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios em um programa?

Gerar números aleatórios em um programa pode ser útil em diversas situações. Alguns exemplos incluem jogos, sorteios, simulações e criptografia. É uma ferramenta poderosa para adicionar imprevisibilidade e variedade em um programa, tornando-o mais dinâmico e interessante.

## Como gerar números aleatórios em C#

Para gerar um número aleatório em C#, utilizamos a classe `Random`, que pode ser encontrada no namespace `System`.

```
C# using System;

// Criando uma instância da classe Random
Random random = new Random();

// Gerando um número aleatório entre 1 e 100
int numeroAleatorio = random.Next(1, 101);

Console.WriteLine($"Número aleatório: {numeroAleatorio}");
```

No código acima, primeiro importamos o namespace `System` para ter acesso à classe `Random`. Em seguida, criamos uma instância da classe e utilizamos o método `Next()` para gerar um número aleatório entre 1 e 100. Por fim, imprimimos o resultado na tela.

## Aprofundando-se em geração de números aleatórios

A geração de números aleatórios não é algo realmente "aleatório", mas sim baseada em um algoritmo matemático. A classe `Random` utiliza o método de congruência linear para gerar uma sequência de números pseudoaleatórios. Isso significa que a sequência gerada não é realmente aleatória, mas possui propriedades estatísticas semelhantes a uma sequência aleatória.

É importante ressaltar que a semente inicial da classe `Random` é baseada no relógio do sistema. Isso significa que se você criar duas instâncias da classe em sequência e não especificar uma semente manualmente, ambas irão gerar a mesma sequência de números. Para evitar isso, é recomendado utilizar uma semente diferente para cada instância, por exemplo, através da hora atual do sistema.

## Veja também

- [Documentação oficial do C# sobre a classe Random](https://docs.microsoft.com/pt-br/dotnet/api/system.random?view=net-5.0)
- [Artigo sobre geração de números aleatórios em C#](https://www.tutorialspoint.com/generate-random-numbers-in-chash)
- [Vídeo explicando como gerar números aleatórios em C#](https://www.youtube.com/watch?v=rODKLpgMe-w)