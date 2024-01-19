---
title:                "Capitalizando uma string"
html_title:           "Java: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Capitalizando Strings em Java

## O Que & Por Que?
Capitalizar uma string é transformar a primeira letra de cada palavra em maiúscula. Programadores fazem isso para melhorar a legibilidade do texto, ou para seguir convenções de nomenclatura.

## Como Fazer:

Vamos ver como fazer isso na prática. Utilize o método `titleCase` que definiremos abaixo:

```Java
public static String titleCase(String texto) {
    String[] palavras = texto.split(" ");
    StringBuilder textoCapitalizado = new StringBuilder();
    
    for (String palavra : palavras) {
        textoCapitalizado.append(Character.toUpperCase(palavra.charAt(0)))
                         .append(palavra.substring(1))
                         .append(" ");
    }
    
    return textoCapitalizado.toString().trim();
}

public static void main(String[] args) {
    String frase = "olá, mundo!";
    System.out.println(titleCase(frase));  // Saida: "Olá, Mundo!"
}
```

## Mergulhando Fundo

O conceito de capitalizar strings existe desde a invenção das primeiras linguagens de programação. Não existe um padrão universal para como lidar com isso, e diferentes linguagens têm diferentes soluções.

Em Java, também podemos utilizar a biblioteca Apache Commons `WordUtils` que fornece a função `capitalize` para capitalizar uma string. No entanto, isso adicionará uma dependência externa ao seu projeto.

A implementação que mostramos acima funciona dividindo a string original em palavras, then capitalizing the first letter of each word and appending all the words together.

## Veja Também

- [Java String Documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Apache Commons Lang API: WordUtils](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/text/WordUtils.html)
- StackOverflow: [How to Capitalize First Letter of Each Word in a String](https://stackoverflow.com/questions/1149855/how-to-upper-case-every-first-letter-of-word-in-a-string)