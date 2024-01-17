---
title:                "Convertendo uma string para caixa baixa."
html_title:           "Java: Convertendo uma string para caixa baixa."
simple_title:         "Convertendo uma string para caixa baixa."
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O que e por que?

Converter uma string para letras minúsculas é um procedimento comum em programação, e é utilizado para garantir que todas as letras em uma string sejam do mesmo caso. Isso pode ser útil para fins de comparação ou para uma saída mais consistente.

## Como fazer:

Para converter uma string para letras minúsculas em Java, podemos usar o método `toLowerCase()`. Veja um exemplo abaixo:

```java
String texto = "Olá Mundo!";
String textoMin = texto.toLowerCase();
System.out.println(textoMin);
```

A saída será "olá mundo!".

## Mergulho profundo:

A conversão de strings para minúsculas tem sido uma prática comum e útil em programação desde os primórdios da linguagem Java. Além do método `toLowerCase()`, também é possível usar a classe `StringBuffer` para converter uma string para minúsculas. Alguns programadores também preferem utilizar a biblioteca Apache Commons para realizar essa conversão, usando sua classe `StringUtils`.

## Veja também:

Para mais informações sobre a conversão de strings para minúsculas em Java, confira a documentação oficial da Oracle: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--

Você também pode se familiarizar com outras manipulações de strings em Java usando as dicas deste artigo: https://www.devmedia.com.br/manipulacao-de-strings-em-java/23426.