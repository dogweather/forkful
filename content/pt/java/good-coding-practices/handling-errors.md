---
title:                "Tratamento de erros"
aliases:
- /pt/java/handling-errors.md
date:                  2024-01-26T00:54:13.994961-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tratamento de erros"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/handling-errors.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

Tratar erros significa escrever código que antecipa e lida com as coisas dando errado. Programadores fazem isso para tornar o software robusto, evitando falhas e comportamentos estranhos.

## Como Fazer:

Java usa exceções para tratar erros. Você envolve o código de risco com um bloco `try` e captura exceções com `catch`. Aqui está um exemplo simples:

```java
public class ExemploTratamentoDeErro {
    public static void main(String[] args) {
        try {
            int resultado = dividir(10, 0);
            System.out.println("O resultado é: " + resultado);
        } catch (ArithmeticException e) {
            System.out.println("Ops, não é possível dividir por zero!");
        }
    }

    private static int dividir(int numerador, int denominador) {
        return numerador / denominador;
    }
}
```

Saída:
```
Ops, não é possível dividir por zero!
```

## Aprofundamento

O tratamento de erros em Java evoluiu. Nos primeiros dias, não havia exceções; programadores verificavam códigos de erro. Depois, o Java introduziu blocos try-catch, permitindo um tratamento de erros mais elegante.

Alternativas ao `try-catch` tradicional incluem `try-with-resources` para fechamento automático de recursos e um código mais limpo, introduzido no Java 7.

Detalhes da implementação importam. Por exemplo, capturar `Exception` ou `Throwable` geralmente é uma má prática. É muito amplo e pode mascarar bugs dos quais você pode não estar ciente. Prefira exceções específicas.

## Veja Também

- Os tutoriais oficiais da Oracle sobre exceções em Java: [https://docs.oracle.com/javase/tutorial/essential/exceptions/](https://docs.oracle.com/javase/tutorial/essential/exceptions/)
- Documentação da declaração `try-with-resources` do Java: [https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html](https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html)
- Effective Java de Joshua Bloch, para as melhores práticas sobre exceções.
