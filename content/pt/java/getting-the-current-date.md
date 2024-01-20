---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:15:04.793835-07:00
html_title:           "Bash: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Pegar a data atual em Java significa acessar o momento presente do sistema. Programadores fazem isso para logar eventos, marcar transações, ou simplesmente para mostrar a data para usuários.

## Como Fazer:

Para pegar a data atual em Java, vamos usar a classe `LocalDate` do pacote `java.time`. Aqui está um exemplo rápido:

```java
import java.time.LocalDate;

public class ExemploDataAtual {
    public static void main(String[] args) {
        LocalDate hoje = LocalDate.now();
        System.out.println("A data de hoje é: " + hoje);
    }
}
```

Rodar esse pedaço de código vai imprimir algo tipo:

```
A data de hoje é: 2023-04-01
```

## Mergulho Profundo:

Antigamente, em versões pré-Java 8, a classe `Date` era comumente utilizada para pegar a data e a hora, mas ela tinha problemas, incluindo questões de design e segurança de thread. Desde Java 8, a API `java.time` foi introduzida para remediar esses problemas, fornecendo classes imutáveis e mais poderosas para a manipulação de data e hora.

Alternativas ao `LocalDate` incluem `LocalDateTime` para data e hora, e `ZonedDateTime` para data e hora com timezone. A escolha depende do contexto do seu problema.

A implementação por trás de `LocalDate.now()` utiliza o relógio do sistema padrão para pegar a data atual. Se precisar de uma data em um fuso horário específico, você pode usar o `ZoneId`:

```java
import java.time.LocalDate;
import java.time.ZoneId;

public class ExemploDataFusoHorario {
    public static void main(String[] args) {
        LocalDate hojeEmTokyo = LocalDate.now(ZoneId.of("Asia/Tokyo"));
        System.out.println("A data de hoje em Tokyo é: " + hojeEmTokyo);
    }
}
```

## Veja Também:
- [Documentação oficial da classe LocalDate](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/time/LocalDate.html)
- [Tutorial da Oracle sobre data e hora](https://docs.oracle.com/javase/tutorial/datetime/)
- [Guia para `java.time` no Baeldung](https://www.baeldung.com/java-8-date-time-intro)