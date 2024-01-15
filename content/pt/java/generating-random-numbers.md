---
title:                "Gerando números aleatórios"
html_title:           "Java: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que usar números aleatórios?

Existem várias razões pelas quais alguém pode querer gerar números aleatórios em seu programa Java. Isso pode ser útil para jogos, testes, criptografia e muito mais. Além disso, a geração de números aleatórios também pode ajudar a tornar seu código mais dinâmico e imprevisível.

## Como gerar números aleatórios em Java

Usando a classe `Random` em Java, podemos facilmente gerar números aleatórios. Primeiro, devemos importar a classe `Random` em nosso código:

```Java
import java.util.Random;
```

Então, podemos criar um objeto `Random` e usar seus métodos para gerar números aleatórios como desejar:

```Java
Random random = new Random();
int numeroAleatorio = random.nextInt(); // gera um número inteiro aleatório
double numeroDecimalAleatorio = random.nextDouble(); // gera um número decimal aleatório
```

Você também pode definir um intervalo para a geração dos números aleatórios, usando os métodos `nextInt()` ou `nextDouble()` com um parâmetro:

```Java
int numeroAleatorioIntervalo = random.nextInt(100); // gera um número inteiro aleatório entre 0 e 99
double numeroDecimalAleatorioIntervalo = random.nextDouble(50.5); // gera um número decimal aleatório entre 0.0 e 50.49999999999999
```

## Mais informações sobre a geração de números aleatórios

É importante notar que os números aleatórios gerados por um computador não são verdadeiramente aleatórios, mas sim pseudoaleatórios. Ou seja, eles são gerados por um algoritmo determinístico e podem ser previstos se o algoritmo for conhecido. No entanto, esses números podem ser tratados como aleatórios na maioria das situações.

Além disso, a classe `Random` também possui métodos para gerar sequências de números aleatórios, definir uma semente para o gerador de números aleatórios e muito mais. Para saber mais sobre esses recursos, você pode consultar a [documentação oficial do Java](https://docs.oracle.com/javase/10/docs/api/java/util/Random.html).

## Veja também

- [Como usar a classe Scanner em Java](https://www.devmedia.com.br/scanner-em-java-como-ler-entrada-do-teclado/28865)
- [Como criar e manipular listas em Java](https://www.devmedia.com.br/trabalhando-com-listas-em-java/29169)
- [Documentação oficial do Java sobre a classe Random](https://docs.oracle.com/javase/10/docs/api/java/util/Random.html)