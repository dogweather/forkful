---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O que é & Porquê?

Converter uma data de uma string é o processo de transformar uma sequência de caracteres em um objeto de data válida em Java. Os programadores fazem isso para facilitar o manuseio de datas e realizar operações relacionadas à data.

## Como fazer:

A classe `SimpleDateFormat` é frequentemente usada para fazer isso. Veja um exemplo:

```Java
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class Main {
    public static void main(String[] args) {
        SimpleDateFormat formatoData = new SimpleDateFormat("dd/MM/yyyy");
        String stringData = "20/12/2020";
        try {
            Date data = formatoData.parse(stringData);
            System.out.println(data);
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }
}
```

Quando você executa esse código, você verá:

```
Sun Dec 20 00:00:00 GMT 2020
```

## Aprofundando

A classe `SimpleDateFormat` foi a maneira tradicional de converter strings em datas, mas com a introdução do Java 8, você tem a opção de usar a API `java.time`. Essa API é imutável (segura para threads) e mais intuitiva. Aqui está um exemplo usando `java.time`:

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class Main {
    public static void main(String[] args) {
        DateTimeFormatter formatoData = DateTimeFormatter.ofPattern("dd/MM/yyyy");
        String stringData = "20/12/2020";
        LocalDate data = LocalDate.parse(stringData, formatoData);
        System.out.println(data);
    }
}
```

E produzirá:

```
2020-12-20
```

## Veja Também

1. [Documentação `java.time`](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
2. [Documentação `SimpleDateFormat`](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
3. [Tutorial da Oracle sobre manipulação de datas e horários](https://docs.oracle.com/javase/tutorial/datetime/index.html)