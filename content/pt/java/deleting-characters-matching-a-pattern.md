---
title:    "Java: Excluindo caracteres correspondentes a um padrão"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, ao trabalhar com strings em Java, precisamos remover caracteres específicos que correspondem a um determinado padrão. Isso pode ser útil ao fazer limpeza de dados ou ao manipular textos em geral.

## Como fazer

Para remover caracteres que correspondem a um padrão em uma string, podemos usar o método `replaceAll()` da classe `java.util.regex.Pattern`. Vamos supor que queremos remover todos os dígitos de uma string. Podemos fazer isso da seguinte maneira:

```Java
String texto = "Nando123Rocks";
texto = texto.replaceAll("[0-9]", ""); // removerá todos os dígitos
System.out.println(texto);
```

O código acima usará a expressão regular `[0-9]` para substituir todos os dígitos na string por uma string vazia, resultando em "NandoRocks" sendo impresso no console.

Podemos usar outras expressões regulares para substituir diferentes padrões de caracteres. Alguns exemplos são:

- `[\s]` para remover todos os espaços em branco
- `[aeiou]` para remover todas as vogais
- `[^a-zA-Z0-9]` para remover todos os caracteres que não sejam letras ou números

## Profundidade

O método `replaceAll()` usa expressões regulares para correspondência de padrão. Isso significa que podemos usar toda a potência das expressões regulares para manipular strings em Java. Por exemplo, podemos usar quantificadores como `*`, `+`, `?` e `{}` para definir padrões mais complexos.

Também podemos usar agrupamentos e grupos de captura para substituir diferentes partes de uma string em uma única operação `replaceAll()`. Este método é muito útil e economiza muito tempo se precisarmos substituir vários padrões em uma string.

## Veja também

- Documentação do método `replaceAll()`: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-
- Tutorial sobre expressões regulares em Java: https://www.devmedia.com.br/utilizando-expressoes-regulares-em-java/27539
- Padrões Java - Guia completo para expressões regulares em Java: https://www.vogella.com/tutorials/JavaRegularExpressions/article.html