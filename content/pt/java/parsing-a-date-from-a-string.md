---
title:                "Analisando uma data a partir de uma string"
date:                  2024-01-20T15:37:05.572921-07:00
html_title:           "Arduino: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Converter strings para datas é como transformar texto brutão em algo que o computador entenda como tempo. Programadores fazem isso para manipular, comparar, armazenar ou mostrar datas de jeitos específicos.

## Como Fazer:

Para converter uma string em uma data em Java, vamos usar a classe `LocalDateTime` e o método `parse` da seguinte maneira:

```java
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class ExemploConversaoData {
    public static void main(String[] args) {
        String dataTexto = "2023-04-01T10:15:30";
        DateTimeFormatter formato = DateTimeFormatter.ISO_LOCAL_DATE_TIME;
        LocalDateTime dataConvertida = LocalDateTime.parse(dataTexto, formato);

        System.out.println("Data convertida: " + dataConvertida);
    }
}
```

Saída esperada:

```
Data convertida: 2023-04-01T10:15:30
```

## Detalhes Profundos:

Antigamente, a manipulação de datas em Java era feita com classes como `Date` e `SimpleDateFormat` da API antiga. Mas elas tinham problemas com thread safety e design confuso. Desde o Java 8, o pacote `java.time` (conhecido como JSR-310) trouxe melhorias gigantescas nessa área. Alternativas? Sim, você pode usar bibliotecas de terceiros como Joda-Time, mas desde o Java 8 não há muita necessidade. Quando parseamos datas, cuidado com padrões (`patterns`), timezones e localidades (`locales`), pois podem transformar uma tarefa simples numa dor de cabeça se ignorados.

## Veja Também:

- A documentação oficial da Oracle sobre a classe `LocalDateTime`: https://docs.oracle.com/javase/10/docs/api/java/time/LocalDateTime.html
- Uma visão geral sobre o pacote `java.time`: https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html
- Para entender melhor sobre timezones e `ZoneId`, recomendamos: https://docs.oracle.com/javase/10/docs/api/java/time/ZoneId.html

Lembrando que, na documentação, você vai encontrar uma infinidade de métodos para tudo quanto é tipo de operação com datas e horas. Caso precise de mais exemplos práticos, comunidades como Stack Overflow têm uma vasta quantidade de discussões sobre o tema.
