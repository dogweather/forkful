---
title:    "Java: Encontrando o comprimento de uma string"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Encontrar o comprimento de uma string é uma habilidade básica e essencial para qualquer programador de Java. Saber como fazer isso pode facilitar a manipulação de dados e otimizar o código em várias situações.

## Como fazer

Para encontrar o comprimento de uma string em Java, usamos o método `length()`. Vamos ver um exemplo:

```Java
String minhaString = "Olá, mundo!";
int comprimento = minhaString.length();
System.out.println("O comprimento da string é: " + comprimento);
```

Neste exemplo, definimos uma string e, em seguida, usamos o método `length()` para encontrar seu comprimento. O valor retornado é armazenado em uma variável inteira e, por fim, é impresso na tela.

Podemos também combinar a chamada do método `length()` com a indexação das strings em Java para encontrar o comprimento de uma substring. Veja o exemplo:

```Java
String minhaString = "Olá, mundo!";
int comprimento = minhaString.substring(0, 3).length(); // encontra o comprimento de "Olá"
System.out.println("O comprimento da substring é: " + comprimento);
```

Neste caso, usamos o método `substring()` para obter a substring "Olá" e, em seguida, usamos o método `length()` para encontrar seu comprimento. Isso pode ser útil quando precisamos trabalhar com partes específicas de uma string.

## Profundidade

Internamente, o método `length()` em Java retorna a quantidade de caracteres armazenados em uma string. No entanto, isso pode ser afetado por diferentes codificações de caracteres. Por exemplo, uma string em UTF-8 pode ter um comprimento diferente do que seria em UTF-16.

Além disso, o método `length()` não conta apenas as letras e números, mas também inclui espaços em branco e caracteres especiais. Isso é importante ter em mente ao trabalhar com strings de entrada do usuário.

## Veja também

- [Documentação oficial do método `length()` em Java](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--)
- [Tutorial sobre manipulação de strings em Java](https://www.w3schools.com/java/java_strings.asp)
- [Artigo sobre codificação de caracteres em Java](https://www.baeldung.com/java-char-encoding)