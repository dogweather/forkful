---
title:                "Java: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Se você é um programador Java, provavelmente já se deparou com a tarefa de buscar e substituir texto em seu código. Isso pode ser necessário para fazer alterações em massa, corrigir erros ou melhorar a legibilidade do seu código. Aprender a realizar essa tarefa pode economizar tempo e melhorar sua eficiência como desenvolvedor.

## Como fazer

Para buscar e substituir texto em Java, você pode usar o método `replace()` da classe `String`. Este método recebe dois parâmetros: o texto a ser substituído e o texto substituto. Veja um exemplo abaixo:

```Java
String texto = "Olá, mundo!";
String novoTexto = texto.replace("mundo", "você");
System.out.println(novoTexto);
```

A saída desse código seria "Olá, você!".

Você também pode usar o método `replaceAll()` para substituir todas as ocorrências de um texto em uma string. Veja um exemplo abaixo:

```Java
String texto = "banana, batata, maçã, abacate";
String novoTexto = texto.replaceAll("a", "e");
System.out.println(novoTexto);
```

A saída seria "benene, betete, meçã, ebecete".

## Mergulho profundo

Ao usar os métodos `replace()` e `replaceAll()`, é importante entender que eles retornam uma nova string e não alteram a string original. Isso significa que se você quiser substituir o texto em uma string e manter a alteração, precisa atribuir o resultado a uma nova string.

Você também pode usar o método `replaceFirst()` para substituir apenas a primeira ocorrência de um texto em uma string. Outra opção é usar expressões regulares para buscar e substituir padrões específicos em uma string.

## Veja também

- [Documentação oficial do Java sobre o método `replace()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replace-char-char-)
- [Tutorial sobre expressões regulares em Java](https://www.devmedia.com.br/expressoes-regulares-em-java/32126)