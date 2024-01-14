---
title:                "Java: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que concatenar strings?

Concatenar strings é uma habilidade importante para programadores de Java, pois permite a combinação de várias strings em uma única string. Isso pode ser útil em situações em que você precisa criar uma string mais complexa, como na criação de mensagens de saída para o usuário ou na construção de consultas de banco de dados.

## Como fazer

Para concatenar strings em Java, usamos o operador de adição (+) para combinar duas ou mais strings em uma única string. Vamos dar uma olhada em um exemplo:

```java
String nome = "Maria";
String sobrenome = "Silva";
String nomeCompleto = nome + " " + sobrenome;
System.out.println(nomeCompleto);
```

Nesse exemplo, temos três strings separadas: "Maria", "Silva" e o resultado da concatenação, "Maria Silva". Observamos que usamos o operador de adição entre as strings e também adicionamos um espaço em branco entre o nome e o sobrenome.

Podemos também usar a classe `StringBuilder` para concatenar strings de maneira mais eficiente, especialmente se estivermos trabalhando com um grande número de strings.

```java
StringBuilder sb = new StringBuilder();
sb.append("Hello");
sb.append(" ");
sb.append("World");
String resultado = sb.toString();
System.out.println(resultado);
```

Nesse exemplo, primeiro criamos uma instância de `StringBuilder` e depois usamos o método `append()` para adicionar cada string e, finalmente, usamos o método `toString()` para obter a string completa.

## Mergulho profundo

É importante notar que ao concatenar strings, uma nova string é criada a cada iteração, o que pode afetar o desempenho do seu programa se estivermos trabalhando com muitas strings. Portanto, é recomendado o uso de `StringBuilder`, pois ele manipula as strings em sua forma mutável, o que é mais eficiente.

Além disso, é possível utilizar o método `concat()` da classe `String` para concatenar duas strings, mas devemos lembrar que esse método cria uma nova instância de `String` a cada iteração e, portanto, é menos eficiente do que o uso do operador de adição ou `StringBuilder`.

## Veja também
- Documentação oficial do Java sobre a classe `StringBuilder`: https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html
- Explicação detalhada sobre concatenação de strings em Java: https://www.geeksforgeeks.org/java-string-concatenation/
- Tutorial em vídeo sobre uso da classe `StringBuilder`: https://www.youtube.com/watch?v=CvzKxFE0tIk