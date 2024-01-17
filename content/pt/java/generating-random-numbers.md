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

## O que e por que? 
Gerar números aleatórios é um processo muito importante na programação em Java. Ele permite que os programadores criem jogos, simulações e outros programas que precisam de uma fonte de aleatoriedade para funcionar corretamente.

## Como fazer: 
Para gerar números aleatórios em Java, usamos a classe "Random" que já está incluída na linguagem. Primeiro, importe a classe usando ```import java.util.Random;```. Em seguida, crie um objeto desta classe com ```Random gerador = new Random();```. Agora, você pode usar métodos como ```nextInt()``` ou ```nextDouble()``` para gerar números inteiros e decimais, respectivamente. Por exemplo, para gerar um número inteiro aleatório entre 1 e 10, podemos usar ```int num = gerador.nextInt(10) + 1;``` e para um número decimal entre 0 e 1, podemos usar ```double num = gerador.nextDouble();```. Veja abaixo um exemplo completo:

```
import java.util.Random;

public class GeradorNumerosAleatorios {
  public static void main(String[] args) {
    Random gerador = new Random();
    int numInteiro = gerador.nextInt(10) + 1;
    double numDecimal = gerador.nextDouble();
    System.out.println("Número inteiro gerado: " + numInteiro);
    System.out.println("Número decimal gerado: " + numDecimal);
  }
}
```

Saída:
```
Número inteiro gerado: 7
Número decimal gerado: 0.522995
```

## Mergulho Profundo:
A geração de números aleatórios é uma técnica amplamente utilizada na computação e pode ser encontrada em diversas linguagens de programação. Em Java, a classe "Random" foi introduzida na versão 1.0 da linguagem e possui diversos métodos para gerar diferentes tipos de números aleatórios. Além disso, existem outras formas de obter números aleatórios em Java, como usando a classe "Math" ou a classe "ThreadLocalRandom".

## Veja também:
- [Overview da classe Random em Java (em inglês)](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Tutorial sobre geração de números aleatórios em Java (em português)](https://www.w3schools.com/java/java_random.asp)