---
title:                "Java: Gerando números aleatórios"
programming_language: "Java"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que Gerar Números Aleatórios em Java

A geração de números aleatórios é uma função importante em linguagens de programação, incluindo Java. A capacidade de gerar números aleatórios pode ser útil em jogos, simulações, criptografia, e muitas outras aplicações. Além disso, pode adicionar um elemento de aleatoriedade e imprevisibilidade aos programas, tornando-os mais interessantes e desafiadores.

## Como Gerar Números Aleatórios em Java

Em Java, podemos gerar números aleatórios usando a classe `java.util.Random`. Aqui está um exemplo simples de um programa que gera 10 números aleatórios:

```Java
import java.util.Random;

public class GeradorAleatorio {
    public static void main(String[] args) {
        Random gerador = new Random(); //cria um objeto Random
        for (int i = 0; i < 10; i++) {
            int numero = gerador.nextInt(100); //gera um número aleatório entre 0 e 99
            System.out.println("Número gerado: " + numero); 
        }
    }
}
```
A saída deste programa pode ser algo como:

```
Número gerado: 76
Número gerado: 29
Número gerado: 85
Número gerado: 51
Número gerado: 99
Número gerado: 19
Número gerado: 32
Número gerado: 70
Número gerado: 58
Número gerado: 2
```

Podemos usar o método `nextInt()` para gerar números inteiros em um determinado intervalo, como no exemplo acima. Também existem outros métodos disponíveis, como `nextDouble()` para gerar números decimais e `nextBoolean()` para gerar valores booleanos. Além disso, podemos definir uma semente (seed) para o objeto `Random` para obtermos uma sequência de números aleatórios determinística.

## Aprofundando-se em Geração de Números Aleatórios

A classe `java.util.Random` usa um algoritmo para gerar números aleatórios, mas é importante notar que esses números não são verdadeiramente aleatórios. Eles são calculados a partir de uma fórmula matemática e podem ser reproduzidos se a semente for a mesma. Se for necessário gerar números verdadeiramente aleatórios, é preciso utilizar uma fonte externa de ruído, como o movimento do mouse ou a temperatura ambiente, para criar uma semente única.

Além disso, é importante lembrar que os números gerados pela classe `Random` seguem uma distribuição uniforme, ou seja, todos têm a mesma probabilidade de serem gerados. Se for necessário uma distribuição diferente, como uma distribuição normal, existem outras classes e métodos disponíveis em Java para isso.

## Veja Também

- [Documentação oficial da classe `Random`](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Tutorial da Oracle sobre geração de números aleatórios em Java](https://docs.oracle.com/javase/tutorial/java/javaOO/random.html)
- [Exemplos de geração de números aleatórios em Java](https://www.baeldung.com/java-random)

*Traduzido de [Java Random Number Generation for Beginners](https://www.baeldung.com/java-random)*