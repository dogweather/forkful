---
title:    "Java: Buscando e substituindo texto"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Muitas vezes, ao escrever um programa em Java, você pode precisar realizar alterações em várias partes de um texto. Em vez de fazer essas alterações manualmente, é mais eficiente utilizar uma função de busca e substituição de texto. Esta técnica pode economizar tempo e facilitar o processo de edição.

## Como fazer

Existem várias maneiras de se realizar uma busca e substituição de texto em Java. Uma delas é utilizando o método `replace()` da classe `String`. Veja um exemplo:

```java
String texto = "Blog de Programação Java";
String novoTexto = texto.replace("Java", "Python");
System.out.println(novoTexto);
```

Este código irá imprimir "Blog de Programação Python", pois a palavra "Java" foi substituída por "Python" no texto original.

Outra opção é utilizar expressões regulares para realizar uma busca mais complexa. Veja um exemplo:

```java
String texto = "123-456-789";
String novoTexto = texto.replaceAll("\\d","X");
System.out.println(novoTexto);
```

Neste caso, o código irá imprimir "XXX-XXX-XXX", pois a expressão regular `\d` irá buscar por qualquer número e substituí-lo por "X".

## Mergulho Profundo

Para entender melhor a funcionalidade de busca e substituição de texto em Java, é importante entender dois conceitos fundamentais: as classes `String` e `StringBuilder`. Ambas possuem métodos para realizar substituições de texto.

Em uma `String`, a substituição é sempre feita criando uma nova `String`, pois ela é imutável. Já em um `StringBuilder`, a substituição é realizada diretamente no próprio objeto, tornando este processo mais eficiente em casos de alterações constantes de texto.

Outra diferença é que o método `replace()` de uma `String` só pode substituir uma sequência de caracteres por outra, enquanto o método `replace()` de um `StringBuilder` permite substituir múltiplas ocorrências ao mesmo tempo.

## Veja Também

- [Documentação oficial do Java sobre substituição de texto](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replace-char-char-)
- [Tutorial de expressões regulares em Java](https://www.devmedia.com.br/expressoes-regulares-em-java/22062)