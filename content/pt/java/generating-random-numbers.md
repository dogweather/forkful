---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:49:19.105752-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Gerar números aleatórios é como jogar dados com o seu computador. Os programadores fazem isso para tudo, desde jogos e simulações até segurança e análise de dados, onde resultados imprevisíveis são essenciais.

## Como Fazer:

Para criar um número aleatório em Java, dê uma olhada nestes exemplos:

```Java
import java.util.Random;

public class GeradorAleatorio {
    public static void main(String[] args) {
        // Cria um gerador de números aleatórios
        Random random = new Random();

        // Gera um número aleatório inteiro
        int numeroAleatorioInteiro = random.nextInt();
        System.out.println("Número aleatório inteiro: " + numeroAleatorioInteiro);

        // Gera um número aleatório entre 0 e 99
        int numeroAleatorioLimitado = random.nextInt(100);
        System.out.println("Número aleatório entre 0 e 99: " + numeroAleatorioLimitado);

        // Gera um número aleatório double
        double numeroAleatorioDouble = random.nextDouble();
        System.out.println("Número aleatório double: " + numeroAleatorioDouble);
    }
}
```

Ao rodar o programa:

```
Número aleatório inteiro: 1915821423
Número aleatório entre 0 e 99: 37
Número aleatório double: 0.7308274565
```

## Mergulho Profundo:

Historicamente, gerar números aleatórios verdadeiros é um desafio. O que os computadores chamam de 'aleatório' geralmente é pseudoaleatório, determinado por um algoritmo. No Java, a classe `java.util.Random` é baseada num algoritmo chamado Linear Congruent Generator (LCG), sendo suficiente para a maioria das necessidades.

Alternativas incluem `SecureRandom` para necessidades de criptografia, e `ThreadLocalRandom` para múltiplas threads. Além disso, a partir do Java 7, você pode usar a classe `java.util.concurrent.ThreadLocalRandom` para melhor desempenho em ambientes concorrentes.

Na implementação, o `nextInt()`, por exemplo, gera um número aleatório dentro do intervalo de inteiros. Já o `nextInt(int bound)` gera um número de 0 (inclusivo) até o valor de 'bound' (exclusivo). Cada método tem detalhes que merecem atenção na documentação oficial.

## Veja Também:

- Documentação oficial da classe Random: [Java SE Random](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- Discussão sobre aleatoriedade e usos em programação: [Stack Overflow](https://stackoverflow.com/questions/tagged/random)
- Artigo sobre segurança e números aleatórios: [SecureRandom JavaDoc](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
