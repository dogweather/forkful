---
title:                "Java: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual no seu código Java?

A obtenção da data atual é uma tarefa comum em muitos projetos de programação, especialmente em aplicações de gerenciamento de tempo ou de agendamento de tarefas. Além disso, compreender como obter a data atual em Java pode ser útil em diversas situações, como registros de auditoria ou para fins de depuração.

## Como obter a data atual em Java

A obtenção da data atual em Java é bastante simples. Basta seguir os passos abaixo:

```
Java.util.Date dataAtual = new Java.util.Date();
System.out.println(dataAtual);
```

A saída do código acima será algo parecido com:

```
Tue Dec 08 12:45:34 GMT 2020
```

Este é o formato padrão para a data e hora atuais em Java. Entretanto, se você deseja exibir a data em um formato específico, como "dia/mês/ano", você pode utilizar a classe SimpleDateFormat, conforme demonstrado abaixo:

```
Java.util.Date dataAtual = new Java.util.Date();
SimpleDateFormat formato = new SimpleDateFormat("dd/MM/yyyy");
System.out.println(formato.format(dataAtual));
```

Agora, a saída será apenas a data, sem hora ou fuso horário:

```
08/12/2020
```

## Aprofundando-se na obtenção da data atual em Java

A classe Java.util.Date é a principal responsável por fornecer a data atual em Java. Entretanto, é importante ressaltar que esta classe não possui métodos para manipular ou formatar a data, apenas contém a data atual como um objeto.

Para manipular e formatar a data, é necessário utilizar a classe SimpleDateFormat. Existem também outras classes, como a Calendar, que podem ser utilizadas para obter e manipular a data atual em Java.

É importante ressaltar que, ao utilizar a classe Date, a data exibida será baseada no fuso horário do seu sistema operacional. Se você deseja exibir a data atual em um fuso horário específico, é recomendado utilizar a classe Calendar ou definir manualmente o fuso horário na formatação da data.

## Veja também
- [Documentação oficial Java.util.Date](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Tutorial sobre Java.util.Date](https://www.tutorialspoint.com/java/util/date_getcurrentmilliseconds.htm)
- [Documentação oficial SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Tutorial sobre formatação de datas em Java](https://www.baeldung.com/java-date-time-format)
- [Documentação oficial Calendar](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)