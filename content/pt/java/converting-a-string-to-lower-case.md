---
title:    "Java: Convertendo uma string para minúsculas"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Há muitas situações em que pode ser necessário converter uma string em minúsculas durante a programação em Java. Isso pode ser útil para comparar strings de forma mais precisa, realizar operações de pesquisa e classificação, e até mesmo para fins de formatação de saída.

## Como fazer

Para converter uma string para minúsculas em Java, podemos usar o método `toLowerCase()` da classe `String`. Ele retornará uma nova string com todos os caracteres em minúsculas. Veja um exemplo abaixo:

```java
String nome = "JOÃO";
String nomeMin = nome.toLowerCase();
System.out.println(nomeMin);
```

O output deste código será `joão`. Note que o método `toLowerCase()` não altera a string original, mas sim retorna uma nova string. 

## Profundidade de Conhecimento

Por baixo dos panos, o método `toLowerCase()` utiliza a tabela de conversão de caracteres ASCII para converter cada caractere maiúsculo em seu equivalente minúsculo. Ele também funciona com caracteres acentuados, convertendo-os para sua forma correspondente em minúsculas.

Uma coisa importante a notar é que a conversão para minúsculas pode não funcionar corretamente para alguns idiomas que têm regras complexas para maiúsculas e minúsculas. Por isso, é sempre importante testar e verificar o resultado em diferentes idiomas.

## Veja também

- [Documentação do método toLowerCase() (em inglês)](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- [Tutorial sobre strings em Java (em português)](https://www.devmedia.com.br/trabalhando-com-strings-em-java/28570)
- [Perguntas frequentes sobre strings em Java (em inglês)](https://www.geeksforgeeks.org/java-string-faq/)