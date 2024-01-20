---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por quê & Para quê?

Gerar números aleatórios significa criar uma sequência de números cujo padrão não pode ser previsto logicamente. Programadores fazem isso para introduzir aleatoriedade em suas aplicações, seja para imitar a realidade (como em jogos de azar) ou para segurança (como na geração de senhas).

## Como fazer:

O Java tem uma classe `java.util.Random` que possuí vários métodos para gerar tipos diferentes de números aleatórios. Aqui estão alguns exemplos:

```Java
import java.util.Random;

public class Main {
    public static void main(String[] args) {
        // criação de objeto
        Random rand = new Random();

        // Gera um número aleatório inteiro
        int numeroInteiro = rand.nextInt();
        System.out.println("Número inteiro aleatório: " + numeroInteiro);

        // Gera um número aleatório de ponto flutuante (float)
        float numeroFloat = rand.nextFloat();
        System.out.println("Número float aleatório: " + numeroFloat);
    }
}
```

E o output será diferente cada vez que você rodar este código.

## Mergulho Profundo

Os primeiros computadores a gerar números aleatórios foram criados na década de 1940. No entanto, a classe `java.util.Random` foi despertada no Java 1.0. É uma classe que usa um algoritmo linear congruente, o que significa que não é segura para criptografia.

Uma alternativa é a classe `java.security.SecureRandom`, que fornece um gerador de números aleatórios (RNG) que tem um nível de segurança mais alto. Apesar de ser mais lento, ele é ideal para situações que exigem alta segurança.

```Java
import java.security.SecureRandom;

public class Main {
    public static void main(String[] args) {
        // Criação de objeto
        SecureRandom rand = new SecureRandom();

        // Gera um número aleatório seguro 
        int secureInt = rand.nextInt();
        System.out.println("Número inteiro aleatório seguro: " + secureInt);
    }
}
```

## Ver Também

1. [Documentação do java.util.Random](https://docs.oracle.com/javase/7/docs/api/java/util/Random.html)
2. [Documentação do java.security.SecureRandom](https://docs.oracle.com/javase/7/docs/api/java/security/SecureRandom.html)