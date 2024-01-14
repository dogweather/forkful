---
title:    "Java: Gerando números aleatórios"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por que

Gerar números aleatórios é uma tarefa importante em programação para diversos propósitos, desde jogos até simulações de Monte Carlo. Com poucas linhas de código, podemos adicionar elementos de aleatoriedade aos nossos programas e torná-los mais dinâmicos e interessantes.

## Como Fazer

```Java
// Importando a classe Random
import java.util.Random;

// Criando um objeto da classe Random
Random rand = new Random();

// Gerando um número inteiro aleatório entre 0 e 10
int num = rand.nextInt(11);
System.out.println("Número aleatório: " + num);

// Gerando um número real aleatório entre 0 e 1
double num2 = rand.nextDouble();
System.out.println("Número aleatório: " + num2);

// Gerando um número longo aleatório entre 0 e 100
long num3 = rand.nextLong(101);
System.out.println("Número aleatório: " + num3);
```

**Saída:**

```
Número aleatório: 7
Número aleatório: 0.562453
Número aleatório: 92
```

A classe `Random` em Java nos permite gerar diferentes tipos de números aleatórios utilizando seus métodos `nextInt()`, `nextDouble()` e `nextLong()`. Podemos especificar o intervalo de números que desejamos gerar passando-o como parâmetro para esses métodos.

## Mergulho Profundo

Quando criamos um objeto da classe `Random`, ele utiliza um valor chamado de "seed" (semente) para gerar os números aleatórios. Por padrão, esse valor é baseado no horário atual do sistema, mas também podemos especificar uma semente específica ao criar o objeto.

Além disso, podemos utilizar a classe `Math` em conjunto com a classe `Random` para gerar números aleatórios com diferentes distribuições, como por exemplo a distribuição normal ou a distribuição uniforme.

Também é importante lembrar que os números gerados pela classe `Random` não são realmente aleatórios, mas sim pseudoaleatórios, pois seguem um algoritmo previsível. Por isso, é importante utilizar outras técnicas para aumentar a complexidade da geração de números e torná-los mais próximos da aleatoriedade.

## Veja Também

- [Java Random Class](https://www.geeksforgeeks.org/java-random-class-examples/)
- [Java Math Class](https://www.tutorialspoint.com/java/lang/math_random.htm)
- [How Random Number Generators Work in Java](https://dzone.com/articles/how-random-number-generators-work-in-java)