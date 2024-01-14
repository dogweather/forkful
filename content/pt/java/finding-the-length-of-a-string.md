---
title:                "Java: Encontrando o comprimento de uma string."
simple_title:         "Encontrando o comprimento de uma string."
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, quando estamos programando em Java, precisamos saber o tamanho de uma string para poder trabalhar com ela de forma eficiente. É por isso que é importante entender como encontrar o comprimento de uma string em Java.

## Como Fazer

Encontrar o comprimento de uma string em Java é muito simples. Basta usar o método `length()` da classe `String`:

```Java
String minhaString = "Olá, mundo!";
int tamanho = minhaString.length();
System.out.println(tamanho); // Output: 12
```

Neste exemplo, declaramos uma variável do tipo `String` chamada `minhaString` com o valor "Olá, mundo!". Em seguida, usamos o método `length()` para encontrar o tamanho da string e atribuí-lo à variável `tamanho`. Por fim, imprimimos o resultado usando o método `println()` da classe `System`.

É importante lembrar que o método `length()` retorna o tamanho da string, incluindo espaços em branco. Se quisermos contar apenas os caracteres da string, podemos usar o método `replace()` para remover os espaços em branco antes de encontrar o tamanho:

```Java
String minhaString = "Olá, mundo!";
int tamanho = minhaString.replace(" ", "").length();
System.out.println(tamanho); // Output: 10
```

## Deep Dive

Internamente, a classe `String` em Java armazena suas strings em um array de caracteres. Para encontrar o comprimento, o método `length()` simplesmente retorna o tamanho do array. Por isso, a complexidade desse método é O(1), o que significa que ele é muito eficiente e rápido.

Também é importante ressaltar que a classe `String` é imutável em Java, ou seja, uma vez que uma string é criada, seu valor não pode ser alterado. Por isso, cada vez que fazemos uma operação que muda o valor de uma string, na verdade estamos criando uma nova e deletando a antiga. Isso pode impactar o desempenho do nosso código, especialmente em casos onde precisamos verificar o comprimento de uma string várias vezes.

## Veja Também

- [Documentação do método length()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--)
- [Tutorial de Strings em Java](https://www.baeldung.com/java-strings)
- [Discussão sobre imutabilidade de Strings em Java](https://stackoverflow.com/questions/8798403/string-is-immutable-what-exactly-is-the-meaning/8798447#8798447)