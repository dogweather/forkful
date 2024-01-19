---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Converter uma data em string é o processo de formatar dados de objeto de data em uma representação textual. Programadores fazem isso principalmente para facilitar a leitura e manipulação da data.

## Como Fazer:

Vamos ver um exemplo usando a classe SimpleDateFormat da API de data e hora do Java.
```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class Main  {
    public static void main(String[] args) {
        SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");
        Date date = new Date();
        String strDate = formatter.format(date);
        System.out.println("Data formatada em String : " + strDate);
    }
}
```
O output do código acima será algo do tipo:
```
Data formatada em String : 27-02-2022 22:40:20
```

## Mais Informações

1. Contexto Histórico: A classe Java SimpleDateFormat tem sido disponível desde o Java 1.1, aprimorando a capacidade de manipulação de datas do Java.
2. Alternativas: Além da classe SimpleDateFormat, existem outras classes, como a LocalDateTime na nova API de data e hora do Java. No entanto, SimpleDateFormat costuma ser a escolha preferida por questões de compatibilidade.
3. Detalhes de Implementação: A classe SimpleDateFormat funciona fornecendo um padrão de string que define como a data deve ser formatada. O exemplo acima usa "dd-MM-yyyy HH:mm:ss", que representa 'dia-mês-ano hora:minuto:segundo'.

## Veja Também

1. [Documentação oficial do SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
2. [Tutorial oficial da Oracle sobre formatação de data](https://docs.oracle.com/javase/tutorial/i18n/format/simpleDateFormat.html)
3. [Discussão na StackOverflow sobre alternativas ao SimpleDateFormat](https://stackoverflow.com/questions/22463062/how-to-parse-format-dates-with-localdatetime-java-8)