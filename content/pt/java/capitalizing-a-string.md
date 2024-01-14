---
title:                "Java: Capitalizar uma string"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma String?

A capitalização de uma String é uma tarefa comum em muitos aplicativos Java, principalmente quando se lida com entradas de usuários. Ela envolve transformar a primeira letra de uma palavra em maiúscula, enquanto mantém o restante em minúsculo. Isso torna o texto mais legível e padronizado, tornando-o uma prática importante em programação.

## Como fazer

Aqui está um exemplo simples de como capitalizar uma String em Java:

```java
public static String capitalizeString(String str) {
    return str.substring(0, 1).toUpperCase() + str.substring(1).toLowerCase();
}

public static void main(String[] args) {
    String nome = "joão";
    System.out.println(capitalizeString(nome)); // Saída: João
}
```

No código acima, criamos um método que recebe uma String e retorna a mesma, porém com a primeira letra maiúscula e o restante em minúsculo. Isso é feito utilizando o método `toUpperCase ()` que converte a primeira letra em maiúscula e o método `toLowerCase ()` que converte as demais letras em minúsculo.

Outra abordagem é utilizar a classe `StringBuilder` para manipular a String:

```java
public static String capitalizeString(String str) {
    StringBuilder sb = new StringBuilder(str);
    sb.setCharAt(0, Character.toUpperCase(sb.charAt(0)));
    return sb.toString();
}

public static void main(String[] args) {
    String nome = "petra";
    System.out.println(capitalizeString(nome)); // Saída: Petra
}
```

Nesse exemplo, criamos um objeto `StringBuilder` a partir da String original e, em seguida, usamos o método `setCharAt ()` para alterar o caractere na posição 0 para maiúsculo.

## Mergulho profundo

Além das abordagens utilizadas anteriormente, na verdade, existem várias formas de capitalizar uma String em Java. Por exemplo, a classe `org.apache.commons.lang3.StringUtils` fornece o método `capitalize ()`, que também pode ser usado para capitalizar uma String.

```Java
public static String capitalizeString(String str) {
    return StringUtils.capitalize(str);
}

public static void main(String[] args) {
    String nome = "aluÍsio";
    System.out.println(capitalizeString(nome)); // Saída: Aluísio
}
```

Esse método também é capaz de lidar com Strings que contêm caracteres especiais, como no exemplo acima.

## Veja também

- [Documentação oficial do Java para o método `toUpperCase ()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toUpperCase--)
- [Documentação oficial do Java para o método `setCharAt ()`](https://docs.oracle.com/javase/8/docs/api/java/lang/CharSequence.html#setCharAt-int-char-)
- [Documentação oficial do Java para a classe `org.apache.commons.lang3.StringUtils`](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html)