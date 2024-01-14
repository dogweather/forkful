---
title:                "C#: Geração de números aleatórios"
simple_title:         "Geração de números aleatórios"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios é importante?

Gerar números aleatórios é uma habilidade essencial para qualquer programador, independentemente da linguagem de programação escolhida. A capacidade de gerar números aleatórios é útil em uma variedade de cenários, desde jogos até a criptografia de dados. É uma técnica fundamental que permite criar uma variedade de aplicações e tornar a programação mais dinâmica e interessante.

## Como gerar números aleatórios em C#

Para gerar números aleatórios em C#, você precisa usar a classe `Random` incorporada na linguagem. Esta classe fornece uma variedade de métodos para gerar números aleatórios baseados em diferentes tipos de dados, como inteiro, ponto flutuante ou booleano. Aqui está um exemplo de código simples que gera um número aleatório entre 1 e 100 e o imprime na tela:

```C#
Random rand = new Random();
int numero = rand.Next(1, 101);
Console.WriteLine(numero);
```

Este código cria uma instância da classe `Random`, usa o método `Next()` para gerar um número inteiro entre 1 e 100 e, em seguida, imprime o número na tela. Você também pode gerar números aleatórios de ponto flutuante usando o método `NextDouble()`, ou até mesmo gerar uma sequência de bytes aleatórios usando o método `NextBytes()`.

## Profundidade na geração de números aleatórios

Ao gerar números aleatórios, é importante entender como os computadores realmente criam esses números. Na verdade, os computadores não são capazes de gerar números realmente aleatórios, pois usam algoritmos e fórmulas matemáticas para gerá-los. No entanto, esses números podem ser considerados "suficientemente aleatórios" para a maioria das aplicações.

É importante também ter cuidado ao usar a função `Random`, pois ela pode parecer aleatória, mas na verdade é baseada em um valor inicial chamado "semente". Se você usar a mesma semente, o mesmo conjunto de números aleatórios será gerado. Portanto, é uma boa prática sempre definir a semente com base em algo que muda com o tempo, como o número de milissegundos desde a meia-noite.

## Veja também

Aqui estão alguns links úteis para aprender mais sobre a geração de números aleatórios em C#:

- [Documentação oficial da Microsoft para a classe Random](https://docs.microsoft.com/pt-br/dotnet/api/system.random?view=netcore-3.1)
- [Tutorial sobre geração de números aleatórios em C#](https://youtu.be/yTwi2lojctQ)
- [Tutorial sobre como tornar os números aleatórios mais seguros em C#](https://youtu.be/XrXOsz3JHXk)

Agora que você aprendeu a gerar números aleatórios em C#, experimente implementá-los em seus próprios projetos e veja como eles podem tornar sua programação mais interessante. Boa sorte!