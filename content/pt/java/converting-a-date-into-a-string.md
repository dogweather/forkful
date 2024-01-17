---
title:                "Convertendo uma data para uma string"
html_title:           "Java: Convertendo uma data para uma string"
simple_title:         "Convertendo uma data para uma string"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?

Converter uma data em uma string é um processo comum em programação, onde uma data é convertida em uma representação legível em texto. Isso pode ser necessário para imprimir a data em um formato específico, armazenar em um banco de dados ou transmitir em uma comunicação. Os programadores fazem isso para facilitar a manipulação e o uso da data em seus códigos.

## Como fazer:

```java
// Importar a classe Date do pacote java.util
import java.util.Date;

// Criar uma nova instância da classe Date
Date data = new Date();

// Converter a data em uma string usando o método toString()
String dataString = data.toString();

// Imprimir a data em sua representação de string
System.out.println("A data é: " + dataString);

// Saída:
// A data é: Wed May 12 14:36:57 BRT 2021
```

## Mergulho profundo:

A conversão de datas em strings tem sido utilizada na programação desde os primórdios, quando as datas eram armazenadas e manipuladas em formato de número. Hoje, existem muitas bibliotecas e frameworks disponíveis que fornecem maneiras mais fáceis e eficientes de converter datas em strings. Alguns exemplos incluem o uso de formatação com a classe SimpleDateFormat e a biblioteca Joda-Time.

## Veja também:

- Documentação oficial da classe Date em Java: https://docs.oracle.com/javase/8/docs/api/java/util/Date.html
- Documentação da classe SimpleDateFormat: https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html
- Biblioteca Joda-Time para manipulação de datas: https://www.joda.org/joda-time/