---
title:                "Saida de depuração de impressão"
html_title:           "Java: Saida de depuração de impressão"
simple_title:         "Saida de depuração de impressão"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/printing-debug-output.md"
---

{{< edit_this_page >}}

# O que & Por quê?

Imprimir saída de depuração é simplesmente o ato de exibir informações úteis durante a execução de um programa. Os programadores fazem isso como uma forma de verificar o funcionamento do código e identificar possíveis erros ou problemas.

# Como fazer:

Para imprimir saída de depuração em Java, você pode usar o método `System.out.println()`, que exibe o conteúdo entre parênteses no console. Aqui está um exemplo:

```java
String mensagem = "Olá mundo!";
System.out.println(mensagem);
```

Você também pode usar o método `System.out.printf()` para formatar a saída de depuração. Por exemplo:

```java
String nome = "Maria";
int idade = 25;
System.out.printf("Olá %s, você tem %d anos.", nome, idade);
```

A saída seria: `Olá Maria, você tem 25 anos.`

# Aprofundando

Impressão de saída de depuração é uma técnica comum na programação, remontando aos primeiros dias da linguagem C. É uma maneira fácil e rápida de verificar se o código está funcionando corretamente. Como alternativa, os programadores também podem usar ferramentas de depuração ou registrar informações em arquivos de log.

Em Java, existem outras opções além de `System.out.println()` e `System.out.printf()`. Por exemplo, você pode usar a classe `java.lang.System.Logger` para gerenciar logs no seu código. Além disso, existem bibliotecas populares como o Log4j e o SLF4J, que oferecem recursos avançados de log e depuração.

Ao imprimir saída de depuração, é importante lembrar de não deixar código de depuração no seu código final. Esses comandos devem ser removidos antes de lançar o aplicativo em produção, pois podem afetar o desempenho e a segurança do seu programa.

# Veja também:

- [Documentação oficial do Java sobre o método println](https://docs.oracle.com/javase/10/docs/api/java/io/PrintStream.html#println(java.lang.String))
- [Tutorial da Oracle sobre impressão de saída de depuração em Java](https://docs.oracle.com/javase/tutorial/essential/io/formatting.html)
- [Página oficial do Log4j](https://logging.apache.org/log4j/2.x/)
- [Página oficial do SLF4J](http://www.slf4j.org/)