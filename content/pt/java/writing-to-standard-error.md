---
title:                "Escrevendo para o erro padrão"
html_title:           "Java: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O Que e Por Que?

Escrever para o erro padrão é uma técnica comum utilizada pelos programadores para lidar com erros e exceções em seus códigos. Isso significa que, em vez de imprimir mensagens de erro para o usuário, o programa irá escrevê-las no console do desenvolvedor. Isso permite que os programadores identifiquem e resolvam problemas em seus códigos de forma mais eficiente.

## Como Fazer:

Um exemplo simples de como escrever para o erro padrão em Java é utilizando o método ```System.err.println()```. Veja o código abaixo:

```
try{
  // códigos que podem gerar um erro ou exceção
} catch(Exception e){
  System.err.println("Ocorreu um erro: " + e.getMessage()); // escreve a mensagem de erro no console do desenvolvedor
}
```

O resultado seria algo parecido com isso:

```
Ocorreu um erro: Algo deu errado
```

## Aprofundando-se:

Essa técnica é amplamente utilizada há muitos anos pelos programadores e é uma forma eficaz de identificar e corrigir problemas em códigos. Além disso, também ajuda a manter a organização e legibilidade do código, já que as mensagens de erro ficam separadas das demais saídas.

Existem outras alternativas para escrever para o erro padrão em Java, como o uso da classe ```System.err``` ou a utilização de bibliotecas externas, como o Log4J. No entanto, o método apresentado anteriormente é o mais simples e amplamente utilizado.

Na implementação, é importante lembrar que o uso excessivo de mensagens de erro pode tornar o código confuso e dificultar a identificação de problemas reais. Portanto, é recomendado utilizá-lo somente em situações necessárias.

## Veja Também:

- [Documentação oficial da classe System](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/System.html)