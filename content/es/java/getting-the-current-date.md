---
title:                "Java: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Porqué Obtener la Fecha Actual en Java

Al escribir código en Java, a menudo necesitamos manejar fechas y horas para realizar ciertas acciones. Obtener la fecha actual puede ser muy útil para realizar cálculos, comparaciones o simplemente para mostrar la fecha en una interfaz de usuario.

#Cómo Obtener la Fecha Actual en Java

Para obtener la fecha actual en Java, podemos utilizar la clase "java.time.LocalDate". Primero, necesitamos importar esta clase en nuestro código:

```
import java.time.LocalDate;
```

Luego, podemos crear una instancia de la clase "LocalDate" utilizando el método "now()", que nos devuelve la fecha actual:

```
LocalDate fechaActual = LocalDate.now();
```

Podemos imprimir la fecha actual en la consola utilizando el método "toString()":

```
System.out.println(fechaActual.toString());
```

Esto producirá la siguiente salida:

```
2021-08-04
```

También podemos obtener valores específicos de la fecha, como el día, el mes y el año, utilizando los métodos "getDayOfMonth()", "getMonth()" y "getYear()":

```
int dia = fechaActual.getDayOfMonth();
int mes = fechaActual.getMonthValue();
int ano = fechaActual.getYear();

System.out.println("Hoy es " + dia + "/" + mes + "/" + ano);
```

La salida sería:

```
Hoy es 04/08/2021
```

# Profundizando en Cómo Obtener la Fecha Actual en Java

La clase "java.time.LocalDate" también nos permite realizar operaciones con fechas, como agregar o restar días, meses o años a una fecha determinada. Podemos utilizar los métodos "plus()" y "minus()" y pasarles como parámetros un objeto "java.time.Period" que representa la cantidad de tiempo que queremos agregar o restar.

También podemos especificar una zona horaria para obtener la fecha y hora actual en una ubicación específica utilizando la clase "java.time.ZonedDateTime". Esto es útil si nuestro código debe funcionar en diferentes zonas horarias y necesitamos mostrar la fecha y hora correctas para cada una.

#Ver También
- Obtener la Fecha y Hora Actual en Java (https://docs.oracle.com/javase/tutorial/datetime/iso/datetime.html)
- Convertir entre diferentes formatos de fechas en Java (https://www.baeldung.com/java-simpledateformat)
- Manipulación avanzada de fechas en Java (https://www.javatpoint.com/java-date-manipulation)