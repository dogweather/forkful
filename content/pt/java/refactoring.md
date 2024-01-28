---
title:                "Refatoração"
date:                  2024-01-26T01:40:18.637946-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refatoração"
programming_language: "Java"
category:             "Java"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/refactoring.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Refactoring é o processo de reestruturação do código de computador existente—mudando a fatoração—sem alterar seu comportamento externo. Programadores fazem isso para melhorar atributos não funcionais do software, melhorando a legibilidade, reduzindo a complexidade e tornando o código mais fácil de manter para futuras empreitadas.

## Como fazer:
Vamos pegar uma classe Java simples que está clamando por refactoring devido a sua má organização e falta de clareza.

```java
public class Calculator {
    public int calc(int op1, int op2, String operation) {
        if (operation.equals("add")) {
            return op1 + op2;
        } else if (operation.equals("subtract")) {
            return op1 - op2;
        } // Outras operações...
    }
}
```

Após o refactoring, temos:

```java
public class Calculator {
    public int add(int operand1, int operand2) {
        return operand1 + operand2;
    }

    public int subtract(int operand1, int operand2) {
        return operand1 - operand2;
    }

    // Outras operações...
}
```

Com o refactoring, melhoramos os nomes de métodos e parâmetros para legibilidade e removemos a necessidade de uma ramificação condicional dentro de um único método. Cada operação agora claramente declara seu propósito.

## Aprofundando:
O refactoring tem suas raízes na comunidade Smalltalk, com sua ênfase na legibilidade do código e no design orientado a objetos, mas realmente decolou no mundo Java no final dos anos 90 e início dos anos 2000, particularmente após a publicação do livro seminal de Martin Fowler, "Refactoring: Improving the Design of Existing Code."

Existem alternativas ao refactoring, como reescrever o código do zero. No entanto, o refactoring é frequentemente preferido porque envolve mudanças incrementais que não interrompem a funcionalidade da aplicação.

Detalhes de implementação ao fazer refactoring em Java (ou em qualquer linguagem de programação) giram em torno de entender os code smells—indicadores de problemas mais profundos no código. Alguns smells incluem métodos longos, classes grandes, código duplicado e uso excessivo de primitivos. Aplicando padrões de refactoring como Extract Method, Move Method, ou Replace Temp with Query, os desenvolvedores podem abordar esses problemas de forma sistemática, garantindo que o código permaneça funcional o tempo todo.

Ferramentas automatizadas, como o suporte ao refactoring do IntelliJ IDEA, ou plugins para Eclipse, podem auxiliar o processo ao automatizar refactorings como renomear variáveis, métodos e classes, extrair métodos ou variáveis, e mover métodos ou classes para diferentes pacotes ou namespaces.

## Veja também:
- "Refactoring: Improving the Design of Existing Code" de Martin Fowler: https://martinfowler.com/books/refactoring.html
- Técnicas de refactoring em Refactoring.Guru: https://refactoring.guru/refactoring/techniques
- Refactoring automatizado no Eclipse: https://www.eclipse.org/eclipse/news/4.18/jdt.php
- Recursos de refactoring do IntelliJ IDEA: https://www.jetbrains.com/idea/features/refactoring.html

Cada um desses recursos fornece uma base para entender os princípios de refactoring ou ferramentas que podem ser aproveitadas para colocar esses princípios em prática.
