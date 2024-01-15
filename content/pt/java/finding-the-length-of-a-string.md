---
title:                "Encontrando o comprimento de uma string"
html_title:           "Java: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Muitas vezes, em programação, precisamos saber o tamanho de uma string para realizar diversas operações. Saber como encontrar o tamanho de uma string é uma habilidade fundamental na programação Java, pois nos permite manipular e usar strings com eficiência.

## Como fazer

Para encontrar o tamanho de uma string em Java, utilizamos o método `length()`, que retorna o número de caracteres da string. Veja o exemplo abaixo:

```Java
String nome = "Maria";
System.out.println(nome.length());
```
Output: 5

No exemplo acima, criamos uma string com o valor "Maria" e utilizamos o método `length()` para encontrar o seu tamanho, que é 5 caracteres. É importante lembrar que os espaços em branco também são contados como caracteres.

Podemos utilizar o método `length()` em conjunto com a estrutura de controle `if` para realizar diferentes ações dependendo do tamanho da string. Veja um exemplo abaixo:

```Java
String palavra = "programar";
if(palavra.length() > 10){
    System.out.println("Essa palavra é muito grande!");
} else {
    System.out.println("Essa palavra é de um tamanho bom.");
}
```
Output: Essa palavra é de um tamanho bom.

## Deep Dive

Além do método `length()` que utilizamos no exemplo anterior, existem outras formas de encontrar o tamanho de uma string em Java.

Uma alternativa é utilizar o método `toCharArray()`, que retorna um array de caracteres da string, e utilizar o método `length` nesse array. Veja um exemplo abaixo:

```Java
String texto = "Olá";
char[] caracteres = texto.toCharArray();
System.out.println(caracteres.length);
```
Output: 3

Podemos utilizar o método `getBytes()` para retornar um array de bytes da string e, em seguida, usar o método `length` nesse array. Veja um exemplo abaixo:

```Java
String frase = "Eu gosto de Java";
byte[] bytes = frase.getBytes();
System.out.println(bytes.length);
```
Output: 17

## Veja também

- Documentação oficial do método `length()` da classe String em Java: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--
- Tutorial sobre strings em Java: https://www.geeksforgeeks.org/java-strings/
- Outros métodos para manipular strings em Java: https://www.baeldung.com/java-string-methods