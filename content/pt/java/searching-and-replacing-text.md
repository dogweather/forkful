---
title:                "Buscando e substituindo texto"
html_title:           "Java: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Você já teve que fazer alterações em um texto extenso e cansou de fazer manualmente? A busca e substituição de texto pode ser uma solução eficiente e rápida para realizar essas tarefas. Além disso, pode poupar tempo e esforço ao lidar com grandes quantidades de dados.

## Como fazer

Para realizar a busca e substituição de texto em um programa Java, você pode utilizar o método `replace()` da classe `String`. Abaixo, um exemplo de código que substitui uma palavra em uma frase:

````Java
String frase = "Java é uma linguagem de programação incrível!";
String novaFrase = frase.replace("incrível", "fantástica");
System.out.println(novaFrase);
````
O resultado será: "Java é uma linguagem de programação fantástica!"

Além disso, é possível utilizar o método `replaceAll()` para substituir todas as ocorrências de uma palavra ou expressão em uma string. Veja o exemplo abaixo:

````Java
String frase = "Hoje é um dia muito bom. Bom mesmo. Bom dia!";
String novaFrase = frase.replaceAll("bom", "ótimo");
System.out.println(novaFrase);
````
O resultado será: "Hoje é um dia muito ótimo. Ótimo mesmo. Ótimo dia!"

## Mergulho profundo

O método `replace()` utiliza expressões regulares para realizar a substituição de texto. Isso significa que você pode utilizar metacaracteres para encontrar padrões específicos e substituí-los. Por exemplo, se você quiser substituir todas as vogais de uma palavra por asteriscos, pode fazer da seguinte forma:

````Java
String palavra = "banana";
String novaPalavra = palavra.replaceAll("[aeiou]", "*");
System.out.println(novaPalavra);
````
O resultado será: "b*n*n*".

Outra funcionalidade interessante é o uso do operador de concatenação `+`, que permite juntar strings e variáveis. Por exemplo:

````Java
String nome = "Carlos";
String novaFrase = "Meu nome é " + nome;
System.out.println(novaFrase);
````
O resultado será: "Meu nome é Carlos".

Existem diversas outras formas de utilizar a busca e substituição de texto em Java, como com o uso de expressões regulares mais complexas e com a utilização de outros métodos da classe `String`. Experimente e explore essas possibilidades para tornar suas tarefas de manipulação de texto mais eficientes e dinâmicas.

## Veja também

- [Documentação oficial do método replace()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replace-char-char-)
- [Tutorial sobre expressões regulares em Java](https://www.devmedia.com.br/expressoes-regulares-em-java-ja-iniciou/6301)