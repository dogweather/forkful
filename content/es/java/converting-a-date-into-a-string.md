---
title:    "Java: Convirtiendo una fecha en una cadena"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por qué

Convertir una fecha en una cadena de texto es una tarea común en programación. Puede ser útil para mostrar la fecha en un formato específico, guardarla en una base de datos o incluso para comparar fechas. Esto es especialmente importante en aplicaciones que manejan información de horarios o eventos.

## Cómo hacerlo

Para convertir una fecha en una cadena de texto en Java, podemos usar la clase `DateFormat` del paquete `java.text`. Primero, necesitamos crear una instancia de esta clase y especificar el formato deseado. A continuación, llamamos al método `format()` y pasamos la fecha que queremos convertir como argumento.

```java 
// Importamos la clase DateFormat
import java.text.DateFormat;

// Creamos una instancia de la clase con el formato deseado
DateFormat formato = DateFormat.getDateInstance(DateFormat.LONG);

// Convertimos la fecha en una cadena de texto
String fechaCadena = formato.format(new Date());

// Imprimimos la cadena resultante
System.out.println(fechaCadena);
```

El resultado será una cadena de texto que contiene la fecha en el formato largo especificado, por ejemplo:

```
13 de marzo de 2021
```

También podemos personalizar el formato de fecha utilizando la clase `SimpleDateFormat`. En este caso, necesitamos especificar un patrón de formato que coincida con la fecha que queremos mostrar. Por ejemplo, si queremos mostrar la fecha en formato "AAAA-MM-DD", usaríamos el patrón "yyyy-MM-dd".

```java
// Importamos la clase SimpleDateFormat
import java.text.SimpleDateFormat;

// Creamos una instancia de la clase con el patrón de formato deseado
SimpleDateFormat formato = new SimpleDateFormat("yyyy-MM-dd");

// Convertimos la fecha en una cadena de texto
String fechaCadena = formato.format(new Date());

// Imprimimos la cadena resultante
System.out.println(fechaCadena);
```

El resultado sería:

```
2021-03-13
```

## Profundizando

Java también ofrece la clase `DateTimeFormatter` en el paquete `java.time` para manejar fechas y horas de una manera más moderna y eficiente. Esta clase utiliza el nuevo API de fecha y hora de Java 8 y nos permite especificar fácilmente el formato deseado mediante el método `ofPattern()`. Además, también podemos manejar diferentes zonas horarias y ubicaciones.

Una de las ventajas de utilizar `DateTimeFormatter` es que es inmutable, lo que significa que no podemos cambiar el valor de un objeto una vez creado. Esto lo hace más seguro para usar en aplicaciones concurrentes.

```java
// Importamos la clase DateTimeFormatter
import java.time.format.DateTimeFormatter;

// Creamos una instancia del objeto con el formato deseado
DateTimeFormatter formato = DateTimeFormatter.ofPattern("dd/MM/yyyy");

// Convertimos la fecha en una cadena de texto
String fechaCadena = formato.format(LocalDate.now());

// Imprimimos la cadena resultante
System.out.println(fechaCadena);
```

El resultado será en el formato "DD/MM/AAAA", por ejemplo:

```
13/03/2021
```

## Vea también

- [Documentación de Java: DateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/DateFormat.html)
- [Documentación de Java: SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Documentación de Java: DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)