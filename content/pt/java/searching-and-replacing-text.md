---
title:                "Java: Buscando e substituindo texto"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Porque
Há muitas vezes em que precisamos alterar algum texto em um programa Java. Pode ser necessário corrigir um erro ortográfico, atualizar uma informação ou simplesmente melhorar a legibilidade do código. A busca e substituição de texto é uma ferramenta útil para tornar essas alterações de maneira rápida e eficiente.

## Como Fazer
Para fazer uma busca e substituição de texto em Java, podemos utilizar o método `replaceAll()` da classe `String`. Este método recebe dois parâmetros: a expressão regular a ser buscada e a string de substituição. Vejamos um exemplo abaixo:

```Java
String texto = "Este texto contém muitas vogais.";
String novoTexto = texto.replaceAll("[aeiou]", "I");
System.out.println(novoTexto);
```
Output:
`IstI tIxtI cIntIm mItIs vIgIIs.`

No exemplo acima, utilizamos uma expressão regular que representa todas as vogais para substituí-las pela letra "I". É importante destacar que o método `replaceAll()` é sensível a maiúsculas e minúsculas, então se quisermos substituir todas as letras "a" e "A", por exemplo, devemos especificar as duas letras na expressão regular.

Também é possível utilizar o método `replace()` se quisermos substituir apenas a primeira ocorrência do padrão de texto. Veja um exemplo abaixo:

```Java
String texto = "Este texto contém muitas vogais.";
String novoTexto = texto.replace("vogais", "consoantes");
System.out.println(novoTexto);
```
Output:
`Este texto contém muitas consoantes.`

## Mergulho Profundo
A busca e substituição de texto em Java pode ser utilizada em casos mais complexos, como por exemplo, no uso de expressões regulares. É possível especificar padrões de texto mais precisos para realizar a substituição. Além disso, também é possível utilizar outros métodos da classe `String`, como `split()` e `Pattern` para obter um maior controle sobre a busca.

## Veja Também
- Documentação do método `replaceAll()` em Java: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#replaceAll(java.lang.String,java.lang.String)
- Tutorial sobre expressões regulares em Java: https://www.regular-expressions.info/tutorial.html