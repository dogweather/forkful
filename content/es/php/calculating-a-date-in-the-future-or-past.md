---
title:                "PHP: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

¿Por qué deberías calcular una fecha en el futuro o pasado?

Calcular fechas en el futuro o pasado es una habilidad esencial para cualquier programador de PHP. Ya sea que estés construyendo una aplicación de agenda, un sistema de reservas o simplemente necesites mostrar una fecha en un formato específico, saber cómo calcular fechas con precisión es crucial para el éxito de tu proyecto.

## Cómo hacerlo

Para calcular una fecha en el futuro o pasado en PHP, podemos utilizar la función `strtotime()` que nos permite trabajar con fechas en formato texto. Esta función toma dos argumentos: una cadena de texto con la fecha que deseamos calcular y un número opcional que representa la cantidad de segundos a agregar o restar.

Veamos un ejemplo de cómo utilizar `strtotime()` para calcular una fecha en el futuro:

```
PHP $today = date("Y-m-d"); //Obtener la fecha actual en formato año-mes-día
echo date("Y-m-d", strtotime("+1 day", $today)); //Imprimirá la fecha de mañana en el mismo formato
```

El segundo argumento `strtotime()` se encarga de agregar 1 día a la fecha actual almacenada en `$today`. Podemos utilizar esta misma función para restar días, meses, años e incluso semanas.

Ahora, si además de mostrar la fecha también deseamos mostrar la hora, podemos utilizar la función `date()` para dar formato a nuestra fecha final.

```
PHP $today = date("Y-m-d H:i:s"); //Obtener la fecha y hora actual en formato año-mes-día hora:minuto:segundo
echo date("Y-m-d H:i:s", strtotime("+3 hours", $today)); //Imprimirá la fecha y hora de dentro de 3 horas
```

## Profundizando

Si deseas calcular una fecha en el futuro o pasado con más precisión, puedes utilizar la función `mktime()` que nos permite construir una fecha a partir de valores específicos como el año, mes, día, hora, minuto y segundo.

Por ejemplo, si deseamos calcular una fecha dentro de 5 días a partir de hoy, podemos hacer lo siguiente:

```
PHP $today = date("Y-m-d"); //Obtener la fecha actual en formato año-mes-día
$future_date = mktime(0, 0, 0, date("m"), date("d") + 5, date("Y")); //Construir una fecha con 5 días adicionales a la fecha actual
echo date("Y-m-d", $future_date); //Imprimirá la fecha dentro de 5 días
```

Puedes jugar con los valores de `mktime()` para construir fechas en el futuro o pasado con diferentes combinaciones de fechas y horas.

## Ver también

- [Documentación oficial de PHP sobre `strtotime()`](https://www.php.net/manual/es/function.strtotime.php)
- [Documentación oficial de PHP sobre `date()`](https://www.php.net/manual/es/function.date.php)
- [Documentación oficial de PHP sobre `mktime()`](https://www.php.net/manual/es/function.mktime.php)