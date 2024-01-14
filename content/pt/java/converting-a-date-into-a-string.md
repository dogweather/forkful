---
title:    "Java: Convertendo uma data em uma string"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por que

Converter uma data em uma string é uma tarefa comum em muitos projetos de programação Java. Isso permite que as datas sejam exibidas de maneira legível para usuários e facilmente armazenadas em bancos de dados.

## Como fazer

Existem várias maneiras de converter uma data em uma string em Java. Uma das maneiras mais fáceis é usar a classe SimpleDateFormat, que permite converter datas em diferentes formatos de string. Veja abaixo um exemplo de código:

```java
SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");
Date data = new Date();
String dataString = sdf.format(data);
System.out.println("Data atual em formato de string: " + dataString);
```

O código acima irá produzir a saída: "Data atual em formato de string: 09/08/2021".

É importante notar que o formato especificado na classe SimpleDateFormat segue um padrão, onde "dd" representa o dia, "MM" o mês e "yyyy" o ano. Além disso, é possível incluir outros caracteres, como barras, hífens e pontos, para formatar a string de acordo com as necessidades do projeto.

## Deep Dive

Ao converter uma data em uma string, é importante estar atento ao uso do fuso horário. Isso pode causar problemas, especialmente quando se trabalha com servidores de diferentes fusos horários. Além disso, é necessário lidar com possíveis exceções que possam ocorrer durante a conversão, como um formato inválido especificado para a data.

Outra dica importante é utilizar a classe Calendar ao invés de Date, pois ela possui mais recursos e é mais adequada para manipular datas de maneira precisa.

## Veja também

- [Documentação oficial do Java - classe SimpleDateFormat (em inglês)](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [Artigo sobre manipulação de datas em Java (em português)](https://www.devmedia.com.br/manipulando-datas-em-java-conheca-a-classe-calendar/20252)