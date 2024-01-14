---
title:    "C#: Geração de números aleatórios"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por que gerar números aleatórios em C#?

Gerar números aleatórios é uma habilidade importante para qualquer programador. Ele permite que você crie variabilidade e imprevisibilidade em seus programas, tornando-os mais dinâmicos e versáteis. Além disso, em determinados cenários, é essencial ter um elemento de aleatoriedade para garantir a segurança e eficiência de certos algoritmos.

## Como fazer isso em C#?

Em C#, a geração de números aleatórios é feita através da classe `Random`. Primeiro, é necessário criar uma instância dessa classe:

```C#
Random random = new Random();
```

Em seguida, é possível utilizar os métodos da classe para gerar números aleatórios de diferentes tipos, como inteiros ou decimais:

```C#
int randomInt = random.Next(); // gera um número inteiro aleatório
double randomDouble = random.NextDouble(); // gera um número decimal aleatório
```

Também é possível definir um intervalo de valores para a geração dos números:

```C#
int randomIntInRange = random.Next(1, 10); // gera um número inteiro entre 1 e 10
```

Além disso, a classe `Random` possui diversos outros métodos que podem ser utilizados para gerar números aleatórios de diferentes formas.

## Mergulho Profundo

Por trás dos bastidores, a geração de números aleatórios em C# utiliza um algoritmo chamado "Mersenne Twister". Esse algoritmo é conhecido por produzir sequências de números aleatórios de alta qualidade e é amplamente utilizado em várias linguagens de programação.

No entanto, é importante lembrar que, por mais aleatórios que esses valores possam parecer, eles são gerados por um computador e, portanto, têm uma natureza determinística. Isso significa que, se você executar o mesmo código várias vezes, obterá a mesma sequência de números aleatórios.

Portanto, é importante ter cuidado ao utilizar esses valores em algoritmos que exigem uma grande dose de aleatoriedade, como jogos de azar ou criptografia.

## Veja Também

- [Documentação da classe Random em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.random?view=net-5.0)
- [Artigo sobre geração de números aleatórios em C#](https://www.devmedia.com.br/geracao-de-numeros-aleatorios-em-csharp/26249)
- [Artigo sobre a natureza determinística dos números aleatórios em programação](https://www.drdobbs.com/a-portable-random-number-generator/184403966)