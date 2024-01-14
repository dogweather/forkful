---
title:                "Java: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/printing-debug-output.md"
---

{{< edit_this_page >}}

Por que imprimir saída de depuração pode ser útil
--
Ao fazer a programação em Java, pode ser útil imprimir a saída de depuração para entender os passos do código e encontrar erros. Isso pode ajudar a identificar qual parte do código está causando problemas e encontrar uma solução mais rapidamente. Além disso, também pode ser utilizado para verificar valores de variáveis em diferentes partes do código.

Como imprimir saída de depuração em Java
--
Para imprimir saída de depuração em Java, podemos usar o método `System.out.println()` seguido de uma mensagem ou expressão que desejamos imprimir. Por exemplo:

```Java
System.out.println("Iniciando o programa de cálculo");
```

Isso imprimirá a mensagem "Iniciando o programa de cálculo" na saída do console. Também podemos imprimir o valor de uma variável:

```Java
int idade = 25;
System.out.println("A idade é: " + idade);
```

Isso imprimirá "A idade é: 25" na saída do console. É importante notar que ao concatenar uma variável com uma String, precisamos usar o operador `+`. Além disso, também podemos imprimir a saída de depuração em diferentes partes do código para verificar a mudança nos valores da variável.

Aprofundando na impressão de saída de depuração
--
Além do método `System.out.println()`, também podemos utilizar outros métodos da classe `System` para imprimir saída de depuração. Algumas opções incluem `System.out.print()` para imprimir sem quebrar linha, `System.err.println()` para imprimir saída de erro, e `System.out.printf()` para formatar a saída. Também podemos usar a biblioteca `java.util.logging` para criar logs de depuração mais detalhados.

Veja também
--
- [Java Tutorial: System.out, System.err, and System.in Streams](https://docs.oracle.com/javase/tutorial/essential/io/cl.html)
- [How to print all variables names and values at debug-time?](https://stackoverflow.com/questions/1554760/how-to-print-all-variables-names-and-values-at-debug-time)
- [Java Debugging with Eclipse - Print and Watch](https://www.javatpoint.com/debugging-by-print-and-watch)