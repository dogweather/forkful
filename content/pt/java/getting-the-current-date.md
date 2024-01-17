---
title:                "Obtendo a data atual"
html_title:           "Java: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O que é & Por quê?

A obtenção da data atual é uma função importante em programação, pois permite que os desenvolvedores obtenham a data e hora exatas no momento em que o código é executado. Isso é particularmente útil para aplicativos que dependem de eventos em tempo real, como agendamento de tarefas ou atualizações de dados.

## Como fazer:

```
Java ... 
import java.util.Date;

public class GetDateExample {

    public static void main(String[] args) {

        // Obtém a data atual
        Date dataAtual = new Date();

        // Exibe a data atual no formato padrão
        System.out.println(dataAtual);

        // Exibe a data atual com formatação personalizada
        System.out.printf("%tB %<te, %<tY at %<tl:%<tM %<Tp", dataAtual);
    }
}
```

Output:
```
Mon Oct 25 12:56:42 EDT 2021
October 25, 2021 at 12:56 PM
```

## Mergulho profundo:

Embora possa parecer uma tarefa simples, obter a data atual envolve vários conceitos de programação, como a utilização de bibliotecas padrão e armazenamento da data em diferentes formatos. Além disso, existem várias alternativas para obter a data atual, incluindo o uso de APIs externas ou servidores NTP. Alguns desenvolvedores também podem optar por usar uma variável de ambiente para armazenar a data atual em vez de chamá-la repetidamente no código.

## Veja também:

- [Documentação oficial do Java sobre a classe Date] (https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/Date.html)
- [Como obter a data atual em diferentes formatos em Java] (https://www.w3resource.com/java-tutorial/date-time/how-to-get-current-date-and-time-in-java.php)
- [Exemplos de uso de APIs externas para obter a data atual] (https://www.baeldung.com/java-date-time)