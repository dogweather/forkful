---
title:    "Java: Gerando números aleatórios"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios pode ser útil

Gerar números aleatórios é uma tarefa comum em programação Java, e pode ser útil em diversas situações. Pode ser utilizado para criar senhas ou códigos de verificação, simular situações em jogos ou em testes de software, entre outras possibilidades.

## Como gerar números aleatórios em Java

Para gerar números aleatórios em Java, podemos utilizar a classe `Random` presente na biblioteca padrão da linguagem. Vamos ver um exemplo de como utilizar essa classe para gerar 10 números aleatórios entre 1 e 100:

```Java
import java.util.Random;

public class RandomExample {

    public static void main(String[] args) {

        // Criando uma instância da classe Random
        Random random = new Random();

        // Gerando e imprimindo 10 números aleatórios entre 1 e 100
        for (int i = 0; i < 10; i++) {
            int randomNumber = random.nextInt(100) + 1; // range de 1 a 100
            System.out.println(randomNumber);
        }
    }
}
```

A saída desse código pode ser algo como:

```
57
12
89
33
45
77
98
6
24
50
```

Além disso, a classe `Random` possui diversos outros métodos que permitem gerar números aleatórios em diferentes formatos, tais como `nextDouble()` para números decimais e `nextBoolean()` para valores booleanos.

## Mergulho profundo: como os números aleatórios são gerados

A geração de números aleatórios em Java se baseia em algoritmos matemáticos que utilizam uma "semente" (um número inicial) e operações matemáticas para gerar uma sequência aparentemente aleatória de números. No entanto, essa sequência não é realmente aleatória, pois pode ser reproduzida se soubermos a semente utilizada e o algoritmo utilizado.

Para evitar esse problema, a classe `Random` utiliza um gerador de números pseudoaleatórios (PRNG) que combina a semente com diferentes fatores, como o tempo do sistema e os valores anteriores gerados. Isso torna a sequência mais imprevisível e difícil de ser reproduzida.

É importante ressaltar que a sequência de números gerada por um PRNG não é realmente aleatória, mas sim "aparentemente" aleatória. Portanto, não deve ser utilizada em situações onde se requer alta segurança ou precisão.

## Veja também

- [Java Random Class Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Random.html)
- [Random number generation in Java](https://www.geeksforgeeks.org/random-number-generation-in-java/)
- [Understanding Random Numbers in Java](https://www.baeldung.com/java-random)