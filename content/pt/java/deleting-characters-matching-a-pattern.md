---
title:                "Java: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que Deletar Caracteres Correspondentes a um Padrão?

Muitas vezes, como programadores, nos deparamos com a necessidade de manipular strings e textos para obter os resultados desejados. Uma das operações mais comuns nessas situações é deletar caracteres correspondentes a um padrão. Isso pode ser necessário para formatar dados de entrada, remover caracteres especiais ou até mesmo filtrar conteúdo indesejado. Neste artigo, vamos explorar como realizar essa tarefa em Java e aprofundar nos conceitos por trás da operação.

## Como Fazer

Para deletar caracteres correspondentes a um padrão em Java, podemos utilizar o método `replaceAll()` da classe `String`. Esse método aceita dois parâmetros: o padrão a ser encontrado e o caractere que irá substituir o padrão. Vamos ver um exemplo prático:

```java
String texto = "Este texto contém muitos números: 1234, 5678, 9123";
String textoFiltrado = texto.replaceAll("[0-9]", "");
System.out.println(textoFiltrado);
```

Neste exemplo, estamos substituindo todos os dígitos numéricos (representados pelo padrão `[0-9]`) por um caractere vazio, resultando em uma string sem números. A saída do código acima será:

`Este texto contém muitos números: , , `

Além disso, podemos utilizar expressões regulares para determinar padrões mais complexos a serem substituídos. Por exemplo, podemos remover todos os caracteres especiais e deixar apenas letras e números em uma string:

```java
String texto = "Olá, !!! amigos !!! 123";
String textoFiltrado = texto.replaceAll("[^a-zA-Z0-9]", "");
System.out.println(textoFiltrado);
```

A saída deste código será:

`Oláamigos123`

## Navegação Profunda

Quando utilizamos o método `replaceAll()` em Java, estamos na verdade utilizando uma classe chamada `Matcher`, que permite encontrar padrões em uma string e modificá-la de acordo. Essa classe utiliza expressões regulares para determinar os padrões a serem encontrados e substituídos.

É importante ter um bom conhecimento de expressões regulares para aproveitar ao máximo o potencial do método `replaceAll()`. Além disso, é possível utilizar métodos como `replaceFirst()` e `replace()` para realizar substituições específicas em uma string.

Outra dica importante é sempre utilizar a classe `StringBuilder` ao manipular strings em Java, já que ela é mais eficiente do que a classe `String`, que é imutável.

## Veja Também

- [Documentação do Método replaceAll() em Java](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#replaceAll(java.lang.String,java.lang.String))
- [Expressões Regulares em Java](https://www.devmedia.com.br/como-utilizar-expressoes-regulares-em-java/27126)
- [Uso da Classe StringBuilder em Java](https://www.devmedia.com.br/melhorando-a-performance-em-java-com-a-classe-stringbuilder/29501)