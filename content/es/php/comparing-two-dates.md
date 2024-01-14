---
title:    "PHP: Comparando dos fechas"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por qué comparar dos fechas en PHP

Comparar dos fechas es una tarea común en la programación web con PHP. Ya sea para validar formularios, realizar cálculos o mostrar información basada en la fecha actual, es esencial tener un buen entendimiento de cómo comparar fechas en PHP correctamente.

## Cómo comparar dos fechas en PHP

¡Afortunadamente, comparar dos fechas en PHP es bastante sencillo! Primero, necesitamos tener nuestras dos fechas en un formato reconocible por PHP. Podemos hacer esto utilizando la función `strtotime` que convierte una fecha en formato de texto en un entero con la representación en segundos desde la época Unix.

Una vez que tenemos nuestras dos fechas en formato de tiempo, podemos utilizar el operador de comparación `>` (mayor que), `<` (menor que) o `==` (igual que) para compararlas.

Por ejemplo, si queremos saber si una fecha es mayor que la otra:

```PHP
$fecha1 = strtotime("10-05-2020");
$fecha2 = strtotime("15-05-2020");

if ($fecha1 > $fecha2) {
  echo "La fecha 1 es mayor que la fecha 2";
} else {
  echo "La fecha 2 es mayor que la fecha 1";
}
```

El resultado sería: "La fecha 2 es mayor que la fecha 1".

## Profundizando en la comparación de fechas

Sin embargo, a veces puede surgir la necesidad de comparar fechas de forma más precisa, teniendo en cuenta horas, minutos y segundos. Para esto, podemos utilizar la clase `DateTime` de PHP. Esta clase nos permite realizar operaciones y comparaciones más complejas con fechas.

Por ejemplo, si queremos saber si una fecha es exactamente igual a otra, incluyendo horas y minutos:

```PHP
$fecha1 = new DateTime("10-05-2020 10:30");
$fecha2 = new DateTime("10-05-2020 10:30");

if ($fecha1 == $fecha2) {
  echo "Las fechas son iguales";
} else {
  echo "Las fechas son diferentes";
}
```

En este caso, el resultado sería: "Las fechas son iguales".

Sin embargo, si solo queremos comparar las fechas sin tener en cuenta el tiempo, podemos utilizar los métodos `setTime` y `setDate` de la clase `DateTime` para establecer una hora y fecha específica, respectivamente, antes de realizar la comparación.

Ahora que tenemos una mejor comprensión de cómo comparar fechas en PHP, podemos implementar esta funcionalidad en nuestro código de manera más eficiente y precisa.

## Ver también

- [Documentación oficial de PHP sobre la clase DateTime](https://www.php.net/manual/es/class.datetime.php)
- [Artículo sobre formatos de fechas en PHP](https://www.php.net/manual/es/datetime.formats.php)
- [Tutorial de comparación de fechas en PHP de TutsPlus](https://code.tutsplus.com/tutorials/dates-and-times-in-php--net-5212)