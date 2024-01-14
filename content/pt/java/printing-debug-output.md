---
title:    "Java: Imprimindo saída de depuração"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Por que imprimir saída de depuração em Java?

Ao escrever um programa Java, é comum deparar-se com problemas de execução ou erros de lógica. Nesses casos, a impressão de saída de depuração pode ser uma ferramenta útil para entender o fluxo do programa e identificar possíveis bugs. Além disso, pode ser uma forma de verificar se os valores das variáveis estão corretos em determinados momentos da execução.

# Como fazer isso:

Para imprimir saída de depuração em Java, utilizamos o método `System.out.println()`, que exibe uma mensagem no console. Por exemplo:

```
System.out.println("Valor da variável x: " + x);
```
O código acima irá imprimir a mensagem "Valor da variável x:" seguida do valor atual da variável x. Podemos também imprimir o conteúdo de mais de uma variável na mesma linha, utilizando o operador de concatenação `+`.

Podemos também utilizar o método `System.out.printf()` para imprimir uma saída formatada, similar ao `printf` da linguagem C. Por exemplo:

```
double valor = 10.25;
System.out.printf("O valor é %.2f", valor);
```
O resultado impresso será "O valor é 10.25", com duas casas decimais após o ponto.

# Mais detalhes:

Além dos métodos `println()` e `printf()`, também podemos utilizar o método `System.out.print()`, que funciona de forma similar ao `println()`, mas não adiciona uma quebra de linha ao final. Isso pode ser útil quando queremos imprimir vários conteúdos na mesma linha.

Outra opção interessante é utilizar uma classe externa de logs, como o `java.util.logging`, que permite criar mensagens de log com níveis de severidade, o que pode ser útil em programas mais complexos. Essa classe também permite direcionar a saída de log para um arquivo, ao invés do console, facilitando a análise posterior.

# Veja também:

- [Documentação do método System.out](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#out)
- [Documentação do método System.out.print](https://docs.oracle.com/javase/8/docs/api/java/io/PrintStream.html#print-boolean-)
- [Tutorial sobre logs em Java](https://www.baeldung.com/java-logging-intro)