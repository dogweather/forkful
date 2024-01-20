---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Comparar duas datas na programação significa verificar se uma data é anterior, posterior ou igual à outra. Esse processo é bastante útil para situações que envolvem agendamentos, temporizações, ou quando qualquer diferenciação baseada no tempo é necessária.

## Como fazer?

Para utilizar, podemos usar a classe `LocalDate` da biblioteca `java.time`. Aqui estão alguns exemplos:

```Java
import java.time.LocalDate;

public class CompararDatas {
    public static void main(String[] args) {
        // Criar 2 diferentes datas
        LocalDate data1 = LocalDate.of(2022, 2, 3);
        LocalDate data2 = LocalDate.of(2022, 1, 20);

        // Verificando se a data1 é após a data2
        if (data1.isAfter(data2)) {
            System.out.println("Data1 é posterior à Data2");
        } else if (data1.isBefore(data2)) {
            // Verificando se a data1 é anterior à data2 
            System.out.println("Data1 é anterior à Data2");
        } else {
            // As datas são iguais
            System.out.println("Data1 é a mesma que Data2");
        }
    }
}
```

Este código irá retornar: `"Data1 é posterior à Data2"` porque 3 de fevereiro de 2022 é posterior a 20 de janeiro de 2022.

## Exploração Profunda

Embora a classe `LocalDate` seja a forma mais fácil e recomendada de comparar datas em Java, há outras opções a considerar:

1. **Contexto Histórico**: Antes do Java 8, a comparação de datas era feita usando as classes `Date` e `Calendar`, mas isso era notoriamente problemático e difícil de usar. A partir do Java 8, as classes `java.time` como `LocalDate` permitem uma comparação mais direta e amigável.

2. **Alternativas**: Pode-se usar a classe `Date` com o seu método `compareTo`. No entanto, a classe `LocalDate`, introduzida no Java 8, é mais segura e oferece mais facilidades.

3. **Detalhes da Implementação**: A classe `LocalDate` tem métodos como `isAfter`, `isBefore` e `isEqual` que facilitam a comparação de datas. Eles retornam verdadeiro ou falso dependendo do resultado.

## Veja Também

Para os leitores interessados, estes são ótimos recursos para aprofundar:

1. Documentação oficial Java para a classe `LocalDate`: www.docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
2. Podcast em português explicando como usar a nova funcionalidade de data do Java 8: www.podcast.java.com.br/8-data-novas-funcionalidades
3. Exemplos detalhados de uso da classe `LocalDate` na prática: www.exemplos.java.com.br/localdate-exemplos