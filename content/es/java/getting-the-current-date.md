---
title:    "Java: Obteniendo la fecha actual"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por qué

Obtener la fecha actual puede parecer una tarea simple, pero es una parte esencial de cualquier programa o aplicación que trabaje con fechas y horarios. Saber cómo obtener la fecha actual en Java puede ayudar a garantizar la precisión y eficiencia en tus proyectos.

## Cómo hacerlo

Hay varias formas de obtener la fecha actual en Java, pero una de las maneras más sencillas es utilizando la clase `java.util.Date`. Para ello, primero debes importarla al comienzo de tu código:

```Java
import java.util.Date;
```

Luego, puedes crear una nueva instancia de `Date` y utilizar el método `toString()` para mostrar la fecha actual en formato de cadena de caracteres:

```Java
Date date = new Date();
System.out.println(date.toString());
```

La salida de este código sería algo como: `Mon Feb 22 17:34:12 CST 2021`.

Otra forma de obtener la fecha actual es utilizando la clase `java.util.Calendar` y su método `getInstance()`. Al igual que antes, primero debes importar la clase:

```Java
import java.util.Calendar;
```

Luego, puedes utilizar el método `getInstance()` para crear una instancia de `Calendar` y obtener la fecha actual en un objeto `Date`:

```Java
Calendar cal = Calendar.getInstance();
Date date = cal.getTime();
System.out.println(date.toString());
```

La salida de este código sería la misma que en el ejemplo anterior.

## Profundizando

Aunque en este artículo hemos visto dos formas de obtener la fecha actual en Java, existen otras opciones como la clase `java.time.LocalDate` y el paquete `java.time` en general, que ofrece una amplia gama de funcionalidades para trabajar con fechas. También es importante tener en cuenta que en la mayoría de los casos es necesario tener en cuenta la zona horaria, ya que la fecha y hora pueden variar dependiendo del lugar en el que se encuentre el usuario. Tener en cuenta estos detalles puede ayudar a evitar posibles errores en tu código.

## Ver también

- [Documentación de Java: Clase Date](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/util/Date.html)
- [Documentación de Java: Paquete time](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/time/package-summary.html)
- [Tutorial de programación en Java: Fechas](https://www.tutorialesprogramacionya.com/javaya/detalleconcepto.php?punto=51&codigo=51&inicio=45)