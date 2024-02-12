---
title:                "Obtendo a data atual"
aliases: - /pt/java/getting-the-current-date.md
date:                  2024-02-03T19:09:44.652994-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obtendo a data atual"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?
Obter a data atual em Java é uma operação fundamental que permite aos programadores manipular objetos de data para operações como registro (logging), cálculos de data e condições baseadas em tempo. É vital em aplicações onde o rastreamento, agendamento e análise de dados temporais são cruciais.

## Como fazer:
Java oferece várias maneiras de obter a data atual, usando tanto a antiga classe `java.util.Date` quanto o mais novo pacote `java.time` (introduzido no Java 8), que é mais versátil e intuitivo.

### Usando `java.time.LocalDate`
```java
import java.time.LocalDate;

public class ExemploDataAtual {
    public static void main(String[] args) {
        LocalDate dataAtual = LocalDate.now();
        System.out.println(dataAtual); // Exemplo de saída: 2023-04-01
    }
}
```
### Usando `java.time.LocalDateTime`
```java
import java.time.LocalDateTime;

public class ExemploDataAtual {
    public static void main(String[] args) {
        LocalDateTime dataHoraAtual = LocalDateTime.now();
        System.out.println(dataHoraAtual); // Exemplo de saída: 2023-04-01T12:34:56.789
    }
}
```
### Usando `java.util.Date` (Legado)
```java
import java.util.Date;

public class ExemploDataAtual {
    public static void main(String[] args) {
        Date dataAtual = new Date();
        System.out.println(dataAtual); // Exemplo de saída: Sat Apr 01 12:34:56 BST 2023
    }
}
```
### Utilizando uma Biblioteca de Terceiros: Joda-Time
Antes do Java 8, Joda-Time era o padrão de facto para data e hora em Java. Se você está trabalhando em sistemas legados ou tem preferência pelo Joda-Time, aqui está como você pode usá-lo para obter a data atual:
```java
import org.joda.time.LocalDate;

public class ExemploDataAtual {
    public static void main(String[] args) {
        LocalDate dataAtual = LocalDate.now();
        System.out.println(dataAtual); // Exemplo de saída: 2023-04-01
    }
}
```
**Nota:** Embora `java.util.Date` e Joda-Time ainda sejam usados, o pacote `java.time` é recomendado para novos projetos devido à sua imutabilidade e API abrangente para manuseio de datas e horas.
