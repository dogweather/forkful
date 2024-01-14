---
title:                "Java: Escrevendo em erro padrão"
simple_title:         "Escrevendo em erro padrão"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##Por que escrever para o erro padrão em Java?

Escrever para o erro padrão, ou standard error, é uma prática comum em Java e outras linguagens de programação. Isso permite que o programador registre e rastreie erros e exceções que ocorrem durante a execução do código. É uma ferramenta valiosa para solucionar problemas e aprimorar a qualidade do código.

##Como fazer?

Existem várias maneiras de escrever para o erro padrão em Java. Um método simples é usar o método `System.err.println()` para imprimir uma mensagem de erro. Por exemplo, se você quiser mostrar uma mensagem de erro quando uma exceção `NullPointerException` ocorrer, o código ficaria assim:

```java
try {
    //código que pode gerar um NullPointerException
} catch (NullPointerException e) {
    System.err.println("Ocorreu um erro: " + e.getMessage());
}
```

Isso imprimirá a mensagem de erro no console, indicando onde exatamente ocorreu a exceção.

Você também pode usar o objeto `System.err` para escrever diretamente no erro padrão. Por exemplo, se você quiser exibir um erro com uma cor vermelha no console, pode usar o código a seguir:

```java
System.err.print("\033[31m"); //cor vermelha
System.err.println("Isto é um erro!"); //mensagem de erro
```

##Aprofundando mais

Além de simplesmente imprimir mensagens de erro, é possível personalizar completamente o tratamento de erros em Java. Você pode criar suas próprias classes de exceção personalizadas para lidar com diferentes tipos de erros e usar a instrução `throw` para lançar essas exceções. Também é possível criar classes de tratamento de exceções para lidar com exceções específicas ou gerais.

Outra dica importante é sempre monitorar o registro de erros e exceções em seu código. Isso permite que você identifique e corrija problemas rapidamente, melhorando a eficiência e a qualidade do seu código. Além disso, é importante também documentar os erros e exceções encontrados e quais as soluções adotadas para resolvê-los, facilitando o trabalho futuro e o entendimento do código por outros programadores.

##Veja também

- [Documentação oficial do Java sobre Tratamento de Exceções](https://docs.oracle.com/javase/tutorial/essential/exceptions/index.html)
- [Tutorial de como usar o erro padrão em Java](https://www.baeldung.com/java-system-err-println)
- [Guia completo de Tratamento de Exceções em Java](https://www.guru99.com/java-exception-handling.html)