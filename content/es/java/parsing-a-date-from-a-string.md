---
title:                "Analizando una fecha a partir de una cadena de texto"
html_title:           "Bash: Analizando una fecha a partir de una cadena de texto"
simple_title:         "Analizando una fecha a partir de una cadena de texto"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La interpretación de una fecha desde una cadena implica convertir un string que representa una fecha en un objeto de fecha real. Los programadores lo hacen para manipular y utilizar fechas de manera más efectiva en sus programas.

## ¿Cómo se hace?

Vamos a utilizar la clase `SimpleDateFormat` de Java. Si tienes un string de fecha como "20/12/2020", puedes convertirlo en un objeto `Date`.

```java
import java.text.SimpleDateFormat;
import java.text.ParseException;
import java.util.Date;

public class Main {
    public static void main(String[] args) {
        SimpleDateFormat format = new SimpleDateFormat("dd/MM/yyyy");
        try {
            Date date = format.parse("20/12/2020");
            System.out.println(date);
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }
}
```

## Análisis Profundo

1. **Contexto histórico:** Java ha proporcionado la capacidad de interpretar fechas desde su versión JDK 1.1. La clase `SimpleDateFormat` permite formatos de fecha personalizados.

2. **Alternativas:** Si estás utilizando Java 8 o una versión superior, puedes utilizar la clase `DateTimeFormatter`.

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class Main {
    public static void main(String[] args) {
        DateTimeFormatter format = DateTimeFormatter.ofPattern("dd/MM/yyyy");
        LocalDate date = LocalDate.parse("20/12/2020", format);
        System.out.println(date);
    }
}
```

3. **Detalles de implementación:** Cuando se utiliza `SimpleDateFormat` o `DateTimeFormatter`, los formatos de fecha son sensibles al idioma y a la región. Por ejemplo, MM/dd/yyyy es común en Estados Unidos mientras que dd/MM/yyyy es común en España.

## Ver También

1. [Documentación oficial de Java para SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)

2. [Documentación oficial de Java para DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)

3. [Convertir String a DateTime en Java](https://www.javatpoint.com/how-to-convert-string-to-datetime-in-java) en JavaTpoint.