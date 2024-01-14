---
title:    "PHP: Calculando una fecha en el futuro o pasado"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## ¿Por qué calcular fechas en el futuro o pasado en PHP?

Calcular fechas en el futuro o pasado es una habilidad crucial para cualquier programador de PHP, ya que a menudo necesitamos manejar datos temporales en nuestras aplicaciones. Esto puede incluir funciones como mostrar la fecha actual, programar eventos en el futuro o registrar entradas en un diario en una fecha específica.

## Cómo hacerlo: Ejemplos de código en PHP

Para calcular fechas en el futuro o pasado en PHP, podemos utilizar la función `date()` junto con la función `strtotime()`. Veamos algunos ejemplos:

```
// Calcular la fecha de mañana
$mañana = date('d/m/Y', strtotime('+1 days'));
echo "Mañana será: $mañana";
// Output: Mañana será: 05/08/2021

// Calcular la fecha de hace una semana
$semana_pasada = date('d/m/Y', strtotime('-1 week'));
echo "Hace una semana fue: $semana_pasada";
// Output: Hace una semana fue: 27/07/2021

// Calcular la fecha en 2 meses y 10 días
$fecha = date('d/m/Y', strtotime('+2 months +10 days'));
echo "Dentro de 2 meses y 10 días será: $fecha";
// Output: Dentro de 2 meses y 10 días será: 15/10/2021
```

Podemos ver que la función `strtotime()` nos permite modificar la fecha actual y calcular una fecha en el futuro o pasado de acuerdo a nuestras necesidades. Podemos utilizar diferentes parámetros como `days`, `months` o `years`, y también podemos combinarlos para obtener una fecha más específica.

## Profundizando en el cálculo de fechas en PHP

La función `date()` acepta dos parámetros: el formato que queremos mostrar y una marca de tiempo opcional. Por defecto, si no se proporciona una marca de tiempo, se utiliza la fecha y hora actual. Sin embargo, la función `strtotime()` nos permite utilizar una marca de tiempo en formato de cadena para calcular una fecha en el pasado o futuro.

También es importante tener en cuenta que la función `strtotime()` solo puede manejar fechas entre 1970 y 2038 debido a limitaciones del sistema operativo. Si necesitamos calcular fechas fuera de este rango, podemos utilizar la clase `DateTime` de PHP.

## Ver también

- [Función `date()` en la documentación de PHP](https://www.php.net/manual/es/function.date.php)
- [Función `strtotime()` en la documentación de PHP](https://www.php.net/manual/es/function.strtotime.php)
- [Clase `DateTime` en la documentación de PHP](https://www.php.net/manual/es/class.datetime.php)