---
title:                "Concatenando strings"
html_title:           "Java: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Você pode precisar combinar duas ou mais strings em um único valor em seu programa Java. Isso pode ser útil para criar mensagens personalizadas, exibir informações formatadas ou construir URLs dinâmicos.

## Como fazer

Para concatenar strings em Java, você pode usar o operador `+` ou o método `concat()`. Aqui estão alguns exemplos:

```java
String str1 = "Olá";
String str2 = "mundo!";

// Usando o operador +
String mensagem = str1 + " " + str2;
System.out.println(mensagem); // Saída: Olá mundo!

// Usando o método concat()
mensagem = str1.concat(" ").concat(str2);
System.out.println(mensagem); // Saída: Olá mundo!
```

Você também pode usar várias strings em uma única operação de concatenação:

```java
String nome = "João";
String sobrenome = "Silva";
int idade = 25;

String perfil = "Meu nome é " + nome + " " + sobrenome + " e tenho " + idade + " anos.";
System.out.println(perfil); // Saída: Meu nome é João Silva e tenho 25 anos.
```

## Aprofundando

Existem algumas coisas importantes a serem consideradas ao concatenar strings em Java:

- O operador `+` é mais fácil de usar e produzirá o mesmo resultado que o método `concat()`.
- Ao concatenar objetos, o Java primeiro converterá todos os valores para strings antes de combinar.
- A concatenação de muitas strings pode ser lenta e consumir muita memória. Se estiver trabalhando com muitas strings, é recomendável usar a classe `StringBuilder` para tornar o processo mais eficiente.

## Veja também

- [Documentação oficial do Java sobre concatenação de strings](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [Tutorial de concatenação de strings em Java](https://www.baeldung.com/java-string-concatenation)
- [Vídeo tutorial sobre concatenação de strings em Java](https://www.youtube.com/watch?v=gHYDjHMie-A)