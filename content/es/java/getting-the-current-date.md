---
title:    "Java: Obteniendo la fecha actual"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Por qué obtener la fecha actual? 

Obtener la fecha actual es una tarea común y necesaria en la programación en Java. Puede ser útil para mostrar la fecha y hora en una aplicación, para realizar cálculos con fechas o simplemente para registrar cuándo se realizó una acción. A continuación, te mostraré cómo obtener la fecha actual en Java.

## Cómo obtener la fecha actual en Java 

Para obtener la fecha actual en Java, utilizaremos la clase `java.util.Date` y `java.util.Calendar`. Ambas clases tienen métodos para obtener la fecha y hora actual en diferentes formatos. Veamos un ejemplo utilizando ambas clases y su salida en la consola.

```Java
import java.util.Date;
import java.util.Calendar;

public class FechaActual {
    public static void main(String[] args) {

        // Obtener la fecha actual utilizando Date
        Date fecha = new Date();
        System.out.println("Fecha actual: " + fecha);

        // Obtener la fecha actual utilizando Calendar
        Calendar calendario = Calendar.getInstance();
        System.out.println("Fecha actual: " + calendario.getTime());
    }
}
```

Con este código, obtenemos la fecha actual en dos formatos diferentes. La salida en la consola será similar a la siguiente:

```
Fecha actual: Mon Oct 18 12:20:36 CEST 2021
Fecha actual: Mon Oct 18 12:20:36 CEST 2021
```

Como se puede ver, ambos métodos nos devuelven la fecha y hora actual en formato de `String`. Sin embargo, podemos personalizar el formato utilizando `SimpleDateFormat`, que nos permite especificar un patrón de formato para mostrar la fecha.

```Java
// Personalizar formato con SimpleDateFormat
SimpleDateFormat formato = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
System.out.println("Fecha actual: " + formato.format(fecha));
```

La salida ahora será similar a la siguiente:

```
Fecha actual: 18/10/2021 12:20:36
```

## Profundizando en la obtención de la fecha actual

Ambas clases, `Date` y `Calendar`, tienen métodos para obtener no solo la fecha actual, sino también la hora, la zona horaria, el día de la semana, el año y mucho más. También hay clases adicionales para trabajar con fechas, como `SimpleDateFormat` o `DateTimeFormatter` que nos permiten formatear y parsear fechas en diferentes formatos.

También es importante tener en cuenta que a partir de Java 8, se introdujo la clase `LocalDate` en el paquete `java.time` que nos ofrece más opciones y una manipulación más sencilla de fechas.

## Ver también

- [Documentación de la clase Date en Java](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Documentación de la clase Calendar en Java](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Tutorial de Java SimpleDateFormat](https://www.programiz.com/java-programming/library/simpledateformat)
- [Java 8 Date and Time API](https://www.baeldung.com/java-8-date-time-intro)