---
title:                "Java: Extraindo subtrings"
simple_title:         "Extraindo subtrings"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings?

Extrair substrings é uma técnica muito útil na programação Java. Com ela, você pode manipular strings de maneira mais fácil e eficiente, obtendo apenas as partes necessárias das strings. Isso pode ser útil em diversas situações, como filtrar dados, validar entradas de usuário e muito mais.

## Como extrair substrings

Para extrair substrings em Java, você pode utilizar o método `substring()` da classe `String`. Este método aceita dois parâmetros: o índice inicial e o índice final da substring que você deseja extrair. Por exemplo:

```Java
String original = "Programação é divertido!";

//Extraindo a substring "divertido"
String substring = original.substring(12, 21);

//Exibindo o resultado
System.out.println(substring); //saída: divertido
```

Você também pode utilizar o método `substring()` para extrair uma substring a partir de um índice específico até o final da string original. Basta informar apenas o índice inicial, como no exemplo a seguir:

```Java
String original = "Java é incrível!";

//Extraindo a substring "incrível"
String substring = original.substring(6);

//Exibindo o resultado
System.out.println(substring); //saída: incrível
```

## Mergulho Profundo

O método `substring()` também pode ser utilizado para extrair caracteres individuais de uma string. Nesse caso, basta informar o mesmo índice para o início e fim da substring desejada. Por exemplo:

```Java
String original = "Mundo";

//Extraindo a substring "d"
String substring = original.substring(3, 4);

//Exibindo o resultado
System.out.println(substring); //saída: d
```

Além disso, é importante lembrar que os índices em Java começam a partir de 0, ou seja, a primeira posição da string tem o índice 0. Portanto, a substring "Mundo" teria os índices 1 e 4, e não 0 e 4, como muitos podem pensar.

## Veja também

- [Documentação do método `substring()`](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int,%20int))
- [Tutorial sobre strings no Java](https://www.devmedia.com.br/trabalhando-com-strings-java/25790)