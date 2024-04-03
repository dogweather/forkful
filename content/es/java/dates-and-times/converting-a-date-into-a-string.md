---
date: 2024-01-20 17:36:52.957251-07:00
description: "(\xBFQu\xE9 y Por Qu\xE9?) Convertir una fecha en una cadena de texto\
  \ significa transformar un objeto `Date` a una representaci\xF3n legible para humanos.\
  \ Los\u2026"
lastmod: '2024-03-13T22:44:58.948810-06:00'
model: gpt-4-1106-preview
summary: "(\xBFQu\xE9 y Por Qu\xE9."
title: Convirtiendo una fecha en una cadena de texto
weight: 28
---

## How to:
(Cómo hacerlo:)

Java proporciona la clase `SimpleDateFormat` para convertir fechas a texto. Aquí está la forma de hacerlo:

```java
import java.text.SimpleDateFormat;
import java.util.Date;

public class FechaComoCadena {
    public static void main(String[] args) {
        Date ahora = new Date();
        SimpleDateFormat formato = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");
        String fechaComoTexto = formato.format(ahora);
        System.out.println(fechaComoTexto);
    }
}
```

Salida de muestra:

```
05-04-2023 16:45:12
```

Desde Java 8 en adelante, puedes usar la API `DateTimeFormatter`:

```java
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class FechaComoCadena {
    public static void main(String[] args) {
        LocalDateTime ahora = LocalDateTime.now();
        DateTimeFormatter formato = DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss");
        String fechaComoTexto = ahora.format(formato);
        System.out.println(fechaComoTexto);
    }
}
```

Salida de muestra:

```
05-04-2023 16:45:12
```

## Deep Dive:
(Buceando más profundo:)

Inicialmente, Java utilizaba la clase `SimpleDateFormat` para formatear fechas, pero tenía problemas con la seguridad de hilos y la inmutabilidad. Con Java 8 se introdujo la API `java.time`, la cual es más segura y flexible. Por ejemplo, `DateTimeFormatter` es inmutable y seguro para hilos.

Alternativas incluyen usar `DateFormat` de Apache Commons Lang o bibliotecas de terceros como Joda-Time, aunque Joda-Time ahora está en modo mantenimiento debido a la nueva API `java.time`.

Detalles de implementación importantes: siempre especifica un `Locale` si tu aplicación será usada en múltiples regiones, para evitar malentendidos con el formato.

## See Also:
(Ver También:)

- Java API Specification for `SimpleDateFormat`: https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html
- Java API Specification for `DateTimeFormatter`: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
- Apache Commons Lang `DateFormatUtils`: https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/time/DateFormatUtils.html
- Joda-Time – Home page: http://www.joda.org/joda-time/
