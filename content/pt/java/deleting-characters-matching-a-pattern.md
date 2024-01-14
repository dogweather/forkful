---
title:                "Java: Excluindo caracteres que correspondem a um padrão."
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que Deletar Caracteres Correspondentes a um Padrão

Há várias razões pelas quais alguém pode querer deletar caracteres que correspondem a um padrão em um programa Java. Pode ser necessário para limpar uma string antes de usar em uma operação, para validar uma entrada do usuário de acordo com um formato específico, ou para substituir caracteres indesejados em um texto.

## Como Fazer

Deletar caracteres correspondentes a um padrão em Java é relativamente simples. Primeiro, é necessário importar a classe `java.util.regex` para que possamos usar expressões regulares em nosso código. Em seguida, podemos utilizar o método `replaceAll()` em uma string para substituir todos os caracteres que correspondem ao padrão especificado por um novo valor.

Um exemplo de código para deletar todos os números de uma string seria o seguinte:

```Java
String texto = "Eu tenho 25 anos de idade.";
String textoSemNumeros = texto.replaceAll("[0-9]", "");
System.out.println(textoSemNumeros);
```

A saída seria: "Eu tenho anos de idade.", já que todos os dígitos de 0 a 9 foram substituídos por uma string vazia.

## Mergulho Profundo

Expressões regulares fornecem uma poderosa ferramenta para lidar com padrões em strings. Com elas, é possível especificar padrões mais complexos, utilizando classes de caracteres como `[A-Z]` para indicar todas as letras maiúsculas e `[a-z]` para indicar todas as letras minúsculas. Também é possível utilizar os chamados "metacaracteres", como `+` para indicar um ou mais ocorrências do caractere anterior e `*` para indicar zero ou mais ocorrências.

Outra funcionalidade interessante das expressões regulares em Java é a capacidade de fazer substituições com grupos de captura. Grupos de captura são porções de um padrão que são delimitados pelos parênteses () e que podem ser referenciadas posteriormente com `$` e o número do grupo. Por exemplo, podemos utilizar o padrão `([A-Z][a-z]+)` para encontrar palavras que comecem com uma letra maiúscula e continuar com uma ou mais letras minúsculas.

Utilizando o mesmo exemplo anterior, podemos substituir todas essas palavras encontradas por um novo valor utilizando o seguinte código:

```Java
String texto = "Eu tenho 25 anos de idade.";
String textoSemPalavras = texto.replaceAll("([A-Z][a-z]+)", "[NOME]");
System.out.println(textoSemPalavras);
```

A saída seria: "Eu tenho [NOME] [NOME] de idade.", já que todas as palavras que seguem a regra especificada foram substituídas pela string `[NOME]`. Isso pode ser útil para ocultar informações sensíveis em um texto, por exemplo.

## Veja também

- [Java Regex Tutorial](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
- [Java API Documentation: java.util.regex](https://docs.oracle.com/javase/8/docs/api/java/util/regex/package-summary.html)
- [Regular-Expressions.info](https://www.regular-expressions.info/)

O uso de expressões regulares pode ser complexo, mas é uma habilidade valiosa para qualquer desenvolvedor Java. Se você precisar lidar com padrões em strings em seus programas, não hesite em usar a classe `java.util.regex` e suas funcionalidades.