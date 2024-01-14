---
title:    "Java: Escrevendo para o erro padrão"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão em Java

Escrever para o erro padrão (stderr) em Java é uma prática muito comum para os programadores. Isso acontece porque a saída do stderr é geralmente utilizada para exibir mensagens de erro ou de informação ao usuário sobre o funcionamento do programa. Além disso, escrever para o stderr é útil para identificar e solucionar problemas durante o processo de desenvolvimento de um código.

## Como fazer em Java

Para escrever para o stderr em Java, podemos utilizar a classe `System.err` e seu método `println()`. Veja um exemplo abaixo:

```Java
System.err.println("Esta é uma mensagem de erro!");
```

Ao executar esse código, a mensagem "Esta é uma mensagem de erro!" será exibida no console como a saída do stderr. É importante lembrar que essa mensagem será exibida em vermelho para facilitar a identificação de erros.

Mas e se quisermos exibir uma mensagem de erro de forma mais específica, como incluir informações adicionais? Podemos utilizar o método `printf()` da classe `System.err`, que permite utilizar formatação de strings. Veja o exemplo abaixo:

```Java
int idade = 25;
String nome = "Maria";
System.err.printf("A %s tem %d anos.", nome, idade);
```

A saída desse código será "A Maria tem 25 anos." no console.

## Mergulho aprofundado

Além do método `println()` e `printf()`, a classe `System.err` possui outros métodos, como `write()` e `print()`, que também podem ser utilizados para escrever para o stderr em Java. Além disso, podemos também utilizar o objeto `System` para acessar o erro padrão através do seu atributo `err`. Vale lembrar que é importante utilizar o stderr para exibir mensagens de erro, enquanto o stdout (saída padrão) é mais adequado para mensagens de informação.

## Veja também

- Documentação oficial do Java sobre a classe `System.err`: https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err
- Tutorial em vídeo sobre como imprimir mensagens de erro em Java: https://www.youtube.com/watch?v=98zD1YsDkJg
- Perguntas frequentes sobre a escrita no stderr em Java: https://stackoverflow.com/questions/18729632/how-to-write-to-standard-error-in-java