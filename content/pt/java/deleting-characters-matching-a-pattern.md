---
title:                "Excluindo caractéres que correspondem a um padrão"
html_title:           "Java: Excluindo caractéres que correspondem a um padrão"
simple_title:         "Excluindo caractéres que correspondem a um padrão"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Existem várias razões pelas quais você pode querer excluir caracteres que correspondem a um determinado padrão em Java, como por exemplo: para garantir que os dados inseridos pelo usuário estejam no formato correto, para limpar dados de entrada contaminados ou para remover caracteres desnecessários de uma string.

## Como fazer

Para excluir caracteres que correspondem a um padrão em Java, você precisará utilizar a classe `String` e o método `replaceAll()`. Este método substitui todas as ocorrências de uma determinada expressão regular por uma string fornecida.

Aqui está um exemplo de código que remove todos os dígitos de uma string e retorna a string resultante:

```Java
String texto = "a1b2c3d4e5f6";
String resultado = texto.replaceAll("\\d", "");
System.out.println(resultado);
```

A saída deste código seria `abcdef`, pois todos os dígitos foram removidos da string original.

## Mergulho Profundo

O método `replaceAll()` utiliza expressões regulares para encontrar os caracteres que devem ser substituídos. Expressões regulares são padrões de texto que podem ser utilizados para realizar operações de busca, substituição e validação em strings.

No exemplo acima, utilizamos `\\d` como a expressão regular, que representa qualquer dígito de 0 a 9. Existem muitos outros metacaracteres que podem ser utilizados em expressões regulares, como `\\w` para representar qualquer caractere alfanumérico e `\\s` para representar qualquer espaço em branco.

É importante lembrar que o método `replaceAll()` é sensível ao caso, o que significa que ele distingue entre letras maiúsculas e minúsculas. Por exemplo, se utilizarmos `\\D` como a expressão regular, apenas caracteres não numéricos serão removidos.

## Veja também

- [Documentação do método `replaceAll()` em Java](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-)
- [Tutorial sobre expressões regulares em Java](https://www.tutorialspoint.com/java/java_regular_expressions.htm)
- [Guia completo de expressões regulares em Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)