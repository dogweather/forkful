---
title:                "Java: Imprimindo saída de depuração"
programming_language: "Java"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração?

Imprimir saída de depuração é uma prática comum entre os desenvolvedores de Java. Ela é útil para entender o fluxo de execução do código e encontrar possíveis erros ou bugs.

## Como fazer

A impressão de saída de depuração em Java é feita utilizando o método "System.out.println()". Este método permite que você imprima mensagens de texto ou variáveis ​​para a saída do sistema.

Um exemplo simples de saída de depuração:

```
Java
int numero = 10;
System.out.println("O número é: " + numero);
```

A saída deste código seria: "O número é: 10". Isso permite que o desenvolvedor verifique se o valor da variável está correto e, se houver algum erro, identificá-lo com mais facilidade.

É importante lembrar de remover todas as saídas de depuração antes de finalizar o código, pois elas podem afetar negativamente o desempenho do programa em produção.

## Detalhes técnicos

Existem algumas técnicas avançadas para imprimir saída de depuração em Java, como o uso de expressões lambda no método "System.out.format()". Este método permite que você imprima valores formatados, facilitando a leitura e análise do código.

Também é possível redirecionar a saída de depuração para arquivos de log, o que pode ser útil para depurar problemas em ambientes de produção.

Além disso, existem bibliotecas de terceiros que podem fornecer funcionalidades adicionais para impressão de saída de depuração, como a biblioteca "Log4j".

## Veja também

- [Documentação oficial do Java para System.out.println()](https://docs.oracle.com/javase/7/docs/api/java/io/PrintStream.html#println())
- [Como imprimir saída de depuração em Java](https://www.baeldung.com/java-print-debugging)
- [Uma visão geral do depurador de Java](https://www2.cs.duke.edu/csed/java/javanotes6/c13/debugger.html)