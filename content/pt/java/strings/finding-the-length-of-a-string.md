---
date: 2024-01-20 17:47:37.851640-07:00
description: "Descobrir o comprimento de uma string significa contar quantos caracteres\
  \ ela tem. Programadores fazem isso para validar entradas, gerenciar formatos de\u2026"
lastmod: '2024-03-13T22:44:46.447358-06:00'
model: gpt-4-1106-preview
summary: Descobrir o comprimento de uma string significa contar quantos caracteres
  ela tem.
title: Descobrindo o comprimento de uma string
weight: 7
---

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
