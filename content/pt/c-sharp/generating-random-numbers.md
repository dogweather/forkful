---
title:    "C#: Gerando números aleatórios"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios é importante

Gerar números aleatórios é essencial em muitos programas de computador. Eles podem ser usados para criar senhas seguras, jogos de azar, testes de desempenho e muito mais. Sem esses recursos, muitas aplicações perderiam sua funcionalidade e utilidade.

## Como fazer isso em C#

Gerar números aleatórios em C# é simples e direto. Basta importar a biblioteca "System.Random" e utilizar o método "Next()" para gerar um número aleatório inteiro. Veja um exemplo abaixo:

```C#
Random gerador = new Random();

int numeroAleatorio = gerador.Next(1, 100);

Console.WriteLine("O número aleatório gerado é: " + numeroAleatorio);
```
Saída: O número aleatório gerado é: 53

Você pode personalizar o intervalo do número gerado, especificando o valor mínimo e máximo dentro dos parênteses do método "Next()". Além disso, também é possível gerar números aleatórios de ponto flutuante utilizando o método "NextDouble()".

## Aprofundando

É importante ressaltar que, apesar de parecerem aleatórios, os números gerados pelos computadores são baseados em um algoritmo matemático que segue uma lógica predefinida. Portanto, não podemos considerá-los completamente aleatórios.

Uma forma de obter uma maior aleatoriedade é utilizar uma semente (seed), que é um valor inicial usado pelo algoritmo de geração de números aleatórios. Podemos definir uma semente personalizada utilizando o construtor da classe "Random", como mostrado abaixo:

```C#
Random gerador = new Random(1234);

int numeroAleatorio = gerador.Next();

Console.WriteLine("O número aleatório gerado é: " + numeroAleatorio);
```
Saída: O número aleatório gerado é: 34521632

Outra opção é utilizar a classe "RNGCryptoServiceProvider", que utiliza um processo de criptografia para gerar números aleatórios mais seguros e pouco previsíveis.

## Veja também

- [Documentação do C#: Random Class](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netcore-3.1)
- [Blog da Microsoft sobre geração de números aleatórios em C#](https://blogs.msdn.microsoft.com/pfxteam/2009/02/28/getting-random-numbers-in-a-thread-safe-way/)