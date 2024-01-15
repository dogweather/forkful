---
title:                "Maiúsculas em uma string"
html_title:           "Java: Maiúsculas em uma string"
simple_title:         "Maiúsculas em uma string"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que

Você pode querer capitalizar uma string para facilitar a leitura e torná-la mais legível. Além disso, em certos casos, como em um sistema de login, é importante garantir que a string inserida pelo usuário corresponda exatamente à string esperada, independentemente de como ela seja inserida (com letras maiúsculas ou minúsculas).

## Como fazer

Para capitalizar uma string em Java, você pode usar o método `toUpperCase()` da classe `String`. Basta passar a string desejada como parâmetro e a função irá retornar a mesma string com todas as letras maiúsculas. Veja um exemplo abaixo:

```Java
String minhaString = "Exemplo de string";
System.out.println(minhaString.toUpperCase());
```

Output:
```
EXEMPLO DE STRING
```

## Mergulho Profundo

Em Java, a classe `String` é imutável, o que significa que ela não pode ser alterada após a sua criação. Portanto, o método `toUpperCase()` não alterará a string original, mas sim retornará uma nova string com as letras maiúsculas.

Caso você queira capitalizar apenas a primeira letra de uma string, pode usar o método `capitalize()` da classe `String`. Outra opção é utilizar a classe `StringBuilder`, que permite a modificação de uma string. Veja um exemplo abaixo:

```Java
String minhaString = "exemplo de string";
System.out.println(minhaString.toUpperCase());
System.out.println(minhaString.capitalize());
```

Output:
```
EXEMPLO DE STRING
Exemplo de string
```

Ambos os métodos `toUpperCase()` e `capitalize()` levam em conta a linguagem padrão do sistema em que o código está sendo executado, ou seja, a capitalização das letras pode variar de acordo com a língua. Além disso, esses métodos também podem funcionar em caracteres especiais e emojis.

## Veja também

- Documentação oficial sobre `String` em Java: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html
- Diferenças entre `String` e `StringBuilder`: https://www.baeldung.com/java-string-vs-stringbuilder-vs-stringbuffer
- Guia completo sobre manipulação de strings em Java: https://www.geeksforgeeks.org/string-vs-stringbuilder-vs-stringbuffer-in-java/