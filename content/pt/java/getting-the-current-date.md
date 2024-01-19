---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que & Por Que? 
Obter a data atual é uma tarefa que envolve recuperar a data e hora atuais de acordo com o sistema ou a zona horária. Programadores fazem isso para registar eventos, marcar transações, gerar relatórios baseados no tempo, entre outros.

## Como Fazer:
Em Java, a classe `LocalDate` do pacote `java.time` nos permite obter a data atual. Aqui está um exemplo:
```Java 
import java.time.LocalDate;

public class Principal {
    public static void main(String[] args) {      
        LocalDate dataAtual = LocalDate.now();
        System.out.println("A data de hoje é: " + dataAtual);
    }
}
```
No `output` você receberá:
```Java
A data de hoje é: 2022-01-04 (por exemplo)
```
## Aprofundando
Historicamente, em versões anteriores do Java, usávamos a classe `java.util.Date` para obter a data e hora atuais. Porém, essa classe é difícil de manipular, levando à introdução do pacote `java.time` no Java 8.
Alternativas incluem classes como `java.time.LocalDateTime` e `java.time.ZonedDateTime`, que fornecem mais detalhes como horário e fuso horário, respectivamente.
Em termos de detalhes de implementação, a classe `LocalDate` recupera a data do relógio do sistema com base no fuso horário padrão.

## Veja Também
Aqui estão alguns links para referências relevantes:
- Documentação oficial da API Java: [java.time.LocalDate] (https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html)
- Tutorial da Oracle sobre a API `java.time`: [Date Time] (https://docs.oracle.com/javase/tutorial/datetime/)
- Stack Overflow para perguntas relacionadas: [java.time] (https://stackoverflow.com/questions/tagged/java.time)