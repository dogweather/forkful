---
date: 2024-01-26 01:10:52.237096-07:00
description: "Como fazer: Aqui est\xE1 um exemplo cl\xE1ssico \u2014 uma fun\xE7\xE3\
  o para calcular o fatorial de um n\xFAmero."
lastmod: '2024-03-13T22:44:46.461499-06:00'
model: gpt-4-1106-preview
summary: "Aqui est\xE1 um exemplo cl\xE1ssico \u2014 uma fun\xE7\xE3o para calcular\
  \ o fatorial de um n\xFAmero."
title: "Organizando o c\xF3digo em fun\xE7\xF5es"
weight: 18
---

## Como fazer:
Aqui está um exemplo clássico — uma função para calcular o fatorial de um número.

```java
public class MathUtils {

    public static void main(String[] args) {
        int number = 5;
        int result = factorial(number);
        System.out.println("Fatorial de " + number + " é: " + result);
    }
    
    public static int factorial(int n) {
        if (n <= 1) {
            return 1;
        }
        return n * factorial(n - 1);
    }
}
```

A saída seria:
```
Fatorial de 5 é: 120
```

## Aprofundamento
Antes das funções existirem, o código era amontoado em blocos monolíticos, tornando a depuração como encontrar uma agulha no palheiro. Agora, encapsular a funcionalidade em funções ajuda a isolar problemas rapidamente. Alternativas incluem expressões lambda em Java ou métodos na programação orientada a objetos, ambos servindo a propósitos semelhantes. Quando você escreve uma função, lembre-se: (1) Cada função deve ter uma única responsabilidade e (2) o nome da função deve descrever claramente seu propósito.

## Veja também
Para mais informações sobre a organização de código:
- Clean Code de Robert C. Martin
- Refactoring: Improving the Design of Existing Code de Martin Fowler
- [Documentação Java da Oracle sobre Definindo Métodos](https://docs.oracle.com/javase/tutorial/java/javaOO/methods.html)
