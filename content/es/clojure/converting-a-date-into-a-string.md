---
title:                "Clojure: Convirtiendo una fecha en una cadena"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

¿Por qué convertir una fecha en una cadena?

Convertir una fecha en una cadena es un proceso común en la programación, especialmente al trabajar con bases de datos o al mostrar información en una interfaz de usuario. Al convertir una fecha en una cadena, podemos hacerla más legible para los usuarios o realizar cálculos matemáticos con ella. ¡Descubre cómo hacerlo en Clojure a continuación!

## Cómo hacerlo

```Clojure
(def date (new java.util.Date)) ; creando una fecha actual
(def str-date (str date)) ; convirtiendo la fecha en una cadena
(println str-date) ; imprimiendo la fecha
```

Salida: "Wed Jun 02 12:00:00 CDT 2021"

Usando la función `str`, podemos convertir fácilmente una fecha en una cadena. Esta función toma cualquier tipo de dato y lo convierte en una cadena. En el ejemplo anterior, usamos la fecha actual como entrada y obtuvimos una cadena con el formato de fecha por defecto. 

Ahora veamos cómo podemos personalizar el formato de la cadena utilizando la función `format`:

```Clojure
(def custom-date (format "hh:mm:ss a MM/dd/yyyy" date)) ; creando una cadena personalizada
(println custom-date) ; imprimiendo la fecha con el formato personalizado
```

Salida: "03:15:30 PM 06/02/2021"

La función `format` utiliza un patrón de formato para especificar cómo queremos que se vea nuestra cadena de fecha. En este caso, hemos utilizado diferentes letras para representar las horas, minutos, segundos, período de tiempo, mes, día y año en ese orden. Puedes encontrar la lista completa de letras y su significado en la documentación oficial de Clojure.

## Deep Dive

En Clojure, las fechas se manejan a través de la clase `java.util.Date`, que es parte de la biblioteca estándar de Java. Esta clase almacena las fechas como un largo, que representa los milisegundos desde la medianoche del 1 de enero de 1970 en UTC. Al convertir una fecha en una cadena, lo que realmente estamos haciendo es mostrar esta información en un formato más legible para los humanos.

Otro aspecto importante a tener en cuenta es que las fechas en Clojure son inmutables, lo que significa que no podemos modificarlas directamente. Sin embargo, podemos utilizar las funciones `setTimeZone` y `setTime` para cambiar el huso horario y la hora respectivamente.

## Ver también

- Documentación oficial de Clojure acerca de fechas y horas: https://clojure.org/reference/dates_and_times
- Página de la función `str` en la documentación oficial: https://clojuredocs.org/clojure.core/str
- Página de la función `format` en la documentación oficial: https://clojuredocs.org/clojure.core/format