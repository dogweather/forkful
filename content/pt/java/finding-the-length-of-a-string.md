---
title:                "Descobrindo o comprimento de uma string"
date:                  2024-01-20T17:47:37.851640-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descobrindo o comprimento de uma string"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Descobrir o comprimento de uma string significa contar quantos caracteres ela tem. Programadores fazem isso para validar entradas, gerenciar formatos de texto ou simplesmente para lidar com dados de forma precisa.

## Como Fazer:

```java
public class Main {
    public static void main(String[] args) {
        String saudacao = "Olá, mundo!";
        int comprimento = saudacao.length();
        System.out.println("O comprimento da string é: " + comprimento);
    }
}
```

Saída:

```
O comprimento da string é: 12
```

## Mergulho Profundo:

Historicamente, a função `length()` faz parte do Java desde suas primeiras versões, facilitando a vida dos desenvolvedores que precisam saber o tamanho de uma sequência de caracteres. Contar caracteres é uma tarefa comum e crucial, especialmente quando se trata de restrições de tamanho ou processamento de texto.

Alternativas para `length()` são raras quando se trata de strings, mas muitas vezes vemos uso de APIs relacionadas, como `size()` para listas ou arrays. Em Java, os Arrays têm um campo `length` sem parênteses por ser um atributo, enquanto `length()` é um método da classe `String`.

Sobre a implementação, o método `length()` da classe `String` retorna o número de caracteres UTF-16 presentes na string. Java armazena strings como uma sequência de valores `char`, que representam valores UTF-16, onde cada `char` é uma unidade de código de 16 bits.

## Veja Também:

- [Java String Documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Java String length() Method – Javatpoint](https://www.javatpoint.com/java-string-length)
- [Unicode and Java char](https://www.oracle.com/technical-resources/articles/javase/supplementary.html)
