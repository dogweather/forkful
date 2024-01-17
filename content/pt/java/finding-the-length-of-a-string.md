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

## O que e Por que?

Encontrar o comprimento de uma string (sequência de caracteres) é uma tarefa comum para os programadores. É simplesmente o processo de determinar quantos caracteres existem em uma determinada string. Os programadores fazem isso para uma variedade de razões, incluindo manipulação de strings, formatação e validação de entradas.

## Como fazer:

```java
// Exemplo 1: Usando o método length () de String
String str = "Olá, mundo!";
int length = str.length();
System.out.println(length); // Output: 12

// Exemplo 2: Usando um loop para contar os caracteres
String str = "Hello, world!";
int length = 0;
for(char c : str.toCharArray()){
    length++;
}
System.out.println(length); // Output: 13
```
## Deep Dive:

O método length () de String foi introduzido na versão Java 1.0 e é a forma mais simples e eficiente de encontrar o comprimento de uma string. Isso é possível porque todas as strings em Java são imutáveis, o que significa que uma vez criada, não podem ser alteradas. Portanto, o comprimento de uma string nunca muda. No entanto, existem outras maneiras de encontrar o comprimento de uma string, como usar um loop ou a classe StringUtils no Apache Commons.

## Veja Também:

- [Documentação Oficial Java: Método length () de String] (https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--)
- [Tutoriais Point: Java - Encontrando o comprimento de uma string] (https://www.tutorialspoint.com/java/java_string_length.htm)
- [Sitepoint: Manipulando strings em Java] (https://www.sitepoint.com/trimming-strings-in-java/)