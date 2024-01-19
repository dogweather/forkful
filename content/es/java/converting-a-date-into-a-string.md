---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Convertir una fecha a una cadena en Java es el proceso de trasformar una instacia de `Date` o `LocalDate` a `String`. Los programadores utilizan esto para la presentación de fechas fácilmente legibles y para el almacenamiento en bases de datos que no admiten el tipo de dato `Date`.

## Cómo hacerlo

```Java
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

public class FechaAString {
    public static void main(String[] args) {
        Date fecha = new Date();

        DateFormat formatoFecha = new SimpleDateFormat("dd-MM-yyyy");
        String cadenaFecha = formatoFecha.format(fecha);

        System.out.println(cadenaFecha);
    }
}
```
La salida del código será algo como:
```
20-12-2024
```

## Profundizando

Históricamente, en las primeras versiones de Java, convertir una fecha a cadena era un poco más complicado. Con las actualizaciones y la adopción de nuevas clases como `SimpleDateFormat`, esto se ha simplifyicado mucho.

Como alternativas, Java 8 introdujo `DateTimeFormatter` que puede ser usado con el nuevo API de fecha y hora. 

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class FechaAStringJava8 {
    public static void main(String[] args) {
        LocalDate fecha = LocalDate.now();

        DateTimeFormatter formatoFecha = DateTimeFormatter.ofPattern("dd-MM-yyyy");
        String cadenaFecha = fecha.format(formatoFecha);

        System.out.println(cadenaFecha);
    }
}
```

Asegúrate de tener en cuenta las diferentes normas de formato de fecha y hora, como la zona horaria, durante la conversión. 

## Ver también

- La documentación oficial de Java para [SimpleDateFormat](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/text/SimpleDateFormat.html)
- La documentación de [DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- Un recurso útil para entender los patrones de formato de fecha y hora en [Java](https://docs.oracle.com/javase/tutorial/i18n/format/simpleDateFormat.html)