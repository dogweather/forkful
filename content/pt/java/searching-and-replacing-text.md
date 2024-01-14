---
title:    "Java: Procurando e substituindo texto"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por que

Quando estamos trabalhando com uma grande quantidade de texto em um programa Java, pode ser necessário fazer alterações específicas. Ao invés de fazer essas mudanças manualmente, podemos usar a técnica de busca e substituição de texto para tornar o processo mais eficiente.

## Como fazer

A busca e substituição de texto em Java é feita através do método `replace()` da classe `String`. Veja um exemplo abaixo:

```Java
String texto = "Essa é uma frase exemplo.";
texto = texto.replace("exemplo", "demonstração");
System.out.println(texto);
```

O resultado desta implementação seria: "Essa é uma frase demonstração."  Note que a variável `texto` foi atualizada com a palavra "demonstração" substituindo "exemplo". 

Também podemos usar expressões regulares para fazer buscas e substituições mais complexas. Por exemplo:

```Java
String texto = "O meu número de telefone é (123)456-7890.";
texto = texto.replaceAll("[^0-9]", "");
System.out.println("Apenas os dígitos: " + texto);
```

Neste exemplo, usamos a expressão regular `[^0-9]` para encontrar todos os caracteres que não são números e substituí-los por uma string vazia. O resultado seria: "1234567890" 

## Aprofundando mais

Além da classe `String`, existem outras classes que também oferecem métodos para busca e substituição de texto em Java, como `StringBuilder` e `StringBuffer`. Estas classes são mais eficientes no que diz respeito à manipulação de strings, especialmente quando se trata de grandes quantidades de texto.

Além disso, podemos realizar buscas e substituições com maior precisão e controle utilizando métodos mais avançados, como `replaceFirst()` e `replaceAll()`. Esses métodos também aceitam expressões regulares, o que nos possibilita fazer alterações mais complexas em nossos textos.

## Veja também

- [Documentação oficial do método replace()](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#replace(char,%20char))
- [Aprenda expressões regulares em Java](https://www.devmedia.com.br/expressoes-regulares-em-java/27490)
- [StringBuilder vs StringBuffer: qual usar em Java?](https://www.devmedia.com.br/stringbuilder-x-stringbuffer-em-java/29348)