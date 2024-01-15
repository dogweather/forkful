---
title:                "Convertendo uma string para letras minúsculas."
html_title:           "Java: Convertendo uma string para letras minúsculas."
simple_title:         "Convertendo uma string para letras minúsculas."
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Há várias razões pelas quais você pode precisar converter uma string para letras minúsculas em seu programa Java. Por exemplo, pode ser necessário comparar strings independentemente de sua capitalização ou pode ser uma etapa necessária no processamento de dados.

## Como Fazer

Para converter uma string para letras minúsculas em Java, você pode usar o método `toLowerCase()` da classe String. Aqui está um exemplo de código:

```
String texto = "EXEMPLO DE STRING";
String textoMinusc = texto.toLowerCase();
System.out.println(textoMinusc);
```

O código acima irá imprimir "exemplo de string" no console. Você também pode aplicar este método diretamente a uma string literal, como em `System.out.println("EXEMPLO DE STRING".toLowerCase());`, que também resultará em "exemplo de string" sendo impresso.

## Mergulho Profundo

O método `toLowerCase()` é sensível ao local do sistema em que o código está sendo executado. Isso significa que, dependendo do idioma e das regras de capitalização desse idioma, a conversão para minúsculas pode ser diferente. Isso pode ser um problema se você estiver manipulando dados multilíngues.

Além disso, é importante lembrar que o método `toLowerCase()` apenas converte caracteres que possuem uma entrada de conversão válida no local do sistema atual. Isso pode causar resultados inesperados se a sua string contiver caracteres especiais ou diacríticos em um idioma diferente do local do sistema.

## Veja também

- Documentação do método `toLowerCase()`: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--
- Artigo sobre manipulação de strings em Java: https://www.devmedia.com.br/manipulando-strings-em-java-texto/25289