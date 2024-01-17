---
title:                "Extraindo subtrings"
html_title:           "Java: Extraindo subtrings"
simple_title:         "Extraindo subtrings"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## O que é e por que fazer?

Extrair substrings pode ser definido como uma tarefa de pegar uma parte específica de uma string maior. Isso pode ser útil para diferentes propósitos, como manipulação de dados e criação de lógicas complexas em programas Java.

## Como fazer:

Para extrair substrings em Java, podemos usar o método `substring()` da classe `String`. Este método tem duas formas: uma que pega uma substring a partir de um índice inicial específico e outra que pega uma substring entre dois índices. Veja os exemplos abaixo:

```Java
String str = "Olá Mundo!"
System.out.println(str.substring(0, 3)); // output: Olá
System.out.println(str.substring(4)); // output: Mundo!
```

## Detalhes técnicos:

Extração de substrings em Java é uma funcionalidade muito utilizada, especialmente em aplicações web e processamento de texto. Essa funcionalidade foi introduzida na versão 1.2 do Java (também conhecida como Java 2) e tem sido aprimorada em versões mais recentes.

Existem também outras formas de extrair substrings em Java, como utilizando as classes `StringBuilder` ou `StringBuffer`, que oferecem melhor performance em cenários de alta concorrência.

Para extrair substrings de forma eficiente, é importante entender como as strings são armazenadas na memória em Java. Vale ressaltar que, por padrão, strings são imutáveis em Java, ou seja, uma vez criadas, elas não podem ser modificadas. Isso significa que cada vez que uma operação de substring é realizada, uma nova string é criada na memória, ocupando mais espaço. Por isso, é importante tomar cuidado com o uso excessivo de operações de substring em programas Java.

## Veja também:

- [Documentação oficial do método substring() em Java](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html#substring(int,int))
- [Tutorial sobre manipulação de strings em Java](https://www.w3schools.com/java/java_strings.asp)
- [Artigo sobre strings imutáveis em Java](https://www.benchresources.net/string-immutable-object-java/)