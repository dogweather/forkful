---
title:                "PHP: Comparando dos fechas"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué

Comparar fechas es una tarea común en la programación para manejar y ordenar datos de manera efectiva. Saber cómo comparar dos fechas te ahorrará tiempo y estrés, y te ayudará a crear código más robusto y preciso.

## Cómo hacerlo

Para comparar dos fechas en PHP, primero debes asegurarte de que ambas estén en el mismo formato, ya sea un string o un timestamp. Luego, puedes utilizar la función `strtotime()` para convertir las fechas a timestamp y así poder realizar la comparación.

A continuación, se muestra un ejemplo de cómo comparar dos fechas en formato de string:

```PHP
// Definir las dos fechas a comparar
$fecha1 = "30-06-2021";
$fecha2 = "15-07-2021";

// Convertir las fechas a timestamp
$timestamp1 = strtotime($fecha1);
$timestamp2 = strtotime($fecha2);

// Comparar las fechas
if ($timestamp1 > $timestamp2) {
    echo "$fecha1 es mayor que $fecha2";
} elseif ($timestamp1 < $timestamp2) {
    echo "$fecha2 es mayor que $fecha1";
} else {
    echo "Ambas fechas son iguales";
}
```

El código anterior imprimirá "15-07-2021 es mayor que 30-06-2021". Ten en cuenta que el operador de comparación utilizado es el símbolo `>`, sin embargo, también se pueden utilizar otros operadores como `>=`, `<=` o `==`, según sea necesario.

## Profundizando

Existen muchas formas de comparar dos fechas en PHP, pero una de las más utilizadas es utilizando la función `date_diff()`, que devuelve la diferencia entre dos fechas en un objeto DateInterval. Esta función es útil cuando se desea obtener detalles más específicos, como la cantidad de días, meses o años entre dos fechas.

Otra función útil para comparar fechas es `checkdate()`, que verifica si una fecha es válida o no. Esto puede ser útil cuando se trabaja con datos ingresados por el usuario, ya que permite evitar errores en la comparación de fechas inválidas.

¡Explora otras funciones y métodos de comparación de fechas en PHP y encuentra el que mejor se adapte a tus necesidades!

## Ver también

- [Documentación oficial de PHP sobre la función date_diff()](https://www.php.net/manual/es/function.date-diff.php)
- [Documentación oficial de PHP sobre la función checkdate()](https://www.php.net/manual/es/function.checkdate.php)
- [Tutorial de W3Schools sobre cómo comparar fechas en PHP](https://www.w3schools.com/php/phptryit.asp?filename=tryphp_func_strtotime)