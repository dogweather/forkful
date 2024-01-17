---
title:                "Buscando e substituindo textos"
html_title:           "Java: Buscando e substituindo textos"
simple_title:         "Buscando e substituindo textos"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O que e por que?

Substituicao de texto e quando voce altera partes especificas de um texto por outras. Isso e util para os programadores porque permite que eles automatizem tarefas repetitivas e economizem tempo.

## Como fazer:

Para substituir texto em Java, voce pode usar o metodo `replaceAll()` da classe `String`. Veja um exemplo abaixo:

```Java
String texto = "Ola, mundo!";
String novoTexto = texto.replaceAll("mundo", "amigo");
System.out.println(novoTexto); // Output: Ola, amigo!
```

## Profundando:

A substituicao de texto tem sido uma ferramenta importante na programacao desde os primeiros dias da linguagem Java. Existem outras maneiras de substituir texto, como o uso da classe `StringBuilder` ou expressoes regulares, mas o metodo `replaceAll()` e a maneira mais simples e direta de fazer isso em Java.

## Veja tambem:

[Documentacao do metodo replaceAll() da classe String](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#replaceAll(java.lang.String,java.lang.String))

[Exemplos de uso do metodo replaceAll()](https://www.baeldung.com/java-string-replaceall)

[Video explicando o metodo replaceAll()](https://www.youtube.com/watch?v=yTYOsNCb6J0)