---
title:    "PHP: Comparando dos fechas"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Por qué comparar fechas en PHP?

Comparar fechas en PHP es una tarea común en la programación web. Puede ser útil en situaciones como validar un formulario con una fecha de vencimiento o mostrar publicaciones en un blog en orden cronológico. En este artículo, aprenderemos cómo comparar dos fechas en PHP y algunos detalles más profundos que pueden ser útiles en tu programación diaria.

## Cómo hacerlo

Para comparar dos fechas en PHP, primero debemos asegurarnos de que ambas estén en formato de fecha válido. Esto se puede hacer utilizando la función `strtotime()`. Esta función toma una cadena de texto que representa una fecha y la convierte en un valor de timestamp, que es un número entero que representa la fecha en segundos desde el 1 de enero de 1970.

Veamos un ejemplo de cómo podemos comparar dos fechas en PHP:

```PHP
<?php
$fecha1 = "2021-01-01"; // Primera fecha
$fecha2 = "2021-02-01"; // Segunda fecha

if (strtotime($fecha1) > strtotime($fecha2)) {
  echo "La primera fecha es posterior a la segunda";
} else {
  echo "La segunda fecha es posterior a la primera";
}
?>
```

En este ejemplo, utilizamos la función `strtotime()` para convertir ambas fechas en valores de timestamp. Luego, comparamos estos valores utilizando una declaración `if` y mostramos un mensaje en consecuencia. Tenga en cuenta que también podemos utilizar los operadores `<` y `==` para comparar las fechas de otras maneras.

Si queremos comparar fechas con una mayor precisión, también podemos utilizar la función `strtotime()` para convertir una fecha en un objeto `DateTime`, que nos permite acceder a métodos útiles para la comparación de fechas. Por ejemplo:

```PHP
<?php
$fecha1 = new DateTime("2021-01-01"); // Primera fecha
$fecha2 = new DateTime("2021-02-01"); // Segunda fecha

if ($fecha1 > $fecha2) {
  echo "La primera fecha es posterior a la segunda";
} else {
  echo "La segunda fecha es posterior a la primera";
}
?>
```

En este caso, utilizamos los objetos `DateTime` para comparar las fechas y mostrar un mensaje adecuado.

## Sumergirse más profundo

Comparar fechas en PHP puede volverse más complicado cuando tenemos en cuenta factores como la zona horaria y la precisión. Por ejemplo, en una infraestructura de servidor distribuido, puede haber diferencias en la configuración de la zona horaria, lo que resulta en resultados inesperados al comparar fechas.

También debemos tener en cuenta que la precisión de una fecha depende del formato en el que la presente. Si utilizamos solo una fecha, se considerará que coincide en la precisión de un día. Sin embargo, si también incluimos la hora y los minutos, la precisión será de segundos.

Por lo tanto, es importante tener en cuenta estas consideraciones al comparar fechas en PHP y tomar medidas adicionales si es necesario en situaciones más complejas.

## Ver también

- [Función strtotime de PHP](https://www.php.net/manual/es/function.strtotime.php)
- [Clase DateTime de PHP](https://www.php.net/manual/es/class.datetime.php)
- [Formatos de fecha y hora en PHP](https://www.php.net/manual/es/datetime.format.php)