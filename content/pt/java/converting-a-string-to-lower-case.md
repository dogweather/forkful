---
title:                "Java: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Por que converter uma string para minúsculas?

Converter uma string para minúsculas pode ser útil em várias situações, como por exemplo, ao fazer comparações de strings, gerar saídas de texto com formatação específica ou simplesmente para deixar o texto mais uniforme e legível.

## Como fazer:

Usando a função `toLowerCase()` da classe `String`, podemos facilmente converter uma string para minúsculas. Veja um exemplo abaixo:

```java
String texto = "Olá, Mundo!";
String textoMinusculo = texto.toLowerCase();
System.out.println(textoMinusculo);
```

A saída será: `olá, mundo!`

Podemos também utilizar esta função em conjunto com outras operações, como por exemplo, comparar duas strings ignorando o seu case, como mostrado no exemplo abaixo:

```java
String texto1 = "Java";
String texto2 = "JAVA";
if (texto1.toLowerCase().equals(texto2.toLowerCase())) {
    System.out.println("As duas strings são iguais!");
} else {
    System.out.println("As duas strings são diferentes.");
}
```

A saída será: `As duas strings são iguais!`

## Aprofundando mais

Ao converter uma string para minúsculas, é importante ter em mente que isso será aplicado em todas as letras da string, incluindo acentos e caracteres especiais, como por exemplo o "ß" (ß é equivalente a "ss" em alemão). Porém, vale ressaltar que a maneira como a conversão é feita pode variar dependendo do idioma utilizado e suas regras de letras maiúsculas e minúsculas.

Além disso, também é importante lembrar que strings em Java são imutáveis, ou seja, ao converter uma string para minúsculas, na verdade estamos criando uma nova string com o texto em minúsculas. Isso pode afetar o desempenho e consumo de memória do seu código, especialmente em casos onde é necessário converter várias strings para minúsculas em um loop.

# Veja também

- [Explicação sobre a classe String em Java](https://www.devmedia.com.br/trabalhando-com-a-classe-string-em-java/25505)
- [Documentação da função toLowerCase()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- [Outras maneiras de transformar strings em Java](https://www.baeldung.com/java-string-lower-upper-case)