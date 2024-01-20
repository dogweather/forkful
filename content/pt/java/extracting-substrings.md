---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

Extraindo substrings significa pegar um trecho de uma "string" ou sequência de caracteres. Os programadores fazem isso para analisar partes específicas de informações, como extrair o nome de um endereço de email.

## Como Fazer:

Aqui está um exemplo:

```Java 

String str = "Olá, Mundo!";
String substr = str.substring(0, 5);
System.out.println(substr);

```

A saída será:

```
Olá, 
```

O método substring() aceita dois parâmetros, o índice inicial e o final (exclusivo). No exemplo acima, pegamos a substring do índice 0 ao 4.

## Aprofundando

(1) No contexto histórico, Java sempre teve a capacidade de extrair substrings. Isso tem sido uma parte essencial do tratamento de texto na linguagem.

(2) Alternativas incluem o uso de expressões regulares para casar e extrair partes mais complexas de uma string.

(3) Vale a pena mencionar que a string original não é alterada quando você extrai uma substring. Isso é porque as strings em Java são imutáveis. 

## Veja Também:

- Documentação oficial da Oracle sobre strings: [Oracle Docs](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- Tutorial mais aprofundado sobre o método substring(): [Tutorial do Método Substring](https://www.baeldung.com/java-string-substring)
- Informações adicionais sobre expressões regulares em Java: [Java Regex Tutorial](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)