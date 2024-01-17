---
title:                "Comparando dos fechas"
html_title:           "PHP: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Comparar dos fechas en programación se refiere a determinar si una fecha es anterior, posterior o igual a otra fecha. Los programadores hacen esto para realizar tareas como ordenar eventos por fecha, calcular la edad de una persona o validar la entrada de fechas en formularios.

## ¿Cómo hacerlo?
Para comparar dos fechas en PHP, se pueden utilizar las funciones nativas ```strtotime()```, ```date_create()``` y ```date_diff()```. Por ejemplo, para comparar si una fecha es anterior a otra, se puede hacer lo siguiente:

```
$fecha1 = strtotime('10-10-2021'); // Se convierte la fecha en un número timestamp
$fecha2 = date_create('12-25-2021'); // Se crea un objeto fecha
if ($fecha1 < $fecha2) {
    echo "La fecha 1 es anterior a la fecha 2";
}
```

El resultado sería:
```
La fecha 1 es anterior a la fecha 2 
```
Existen varias formas de comparar fechas en PHP, como utilizando la función ```strtotime()``` con ```strcmp()``` o utilizando operadores de comparación como ```<``` y ```>```. Depende del contexto y preferencias del programador.

## En profundidad
Antes de la llegada de PHP 5.3, no había una forma sencilla de comparar fechas en PHP. Se debía convertir las fechas a un formato legible para el sistema, como un número timestamp, para poder realizar la comparación.

Una alternativa a las funciones nativas de PHP para comparar fechas es utilizar la librería Carbon, que permite manejar fechas y realizar operaciones con ellas de forma sencilla y legible.

Para una implementación detallada sobre cómo comparar fechas en PHP, se pueden consultar documentaciones y tutoriales en línea, así como ejemplos en páginas como GitHub o Stack Overflow.

## Ver también
- Documentación oficial de PHP sobre fechas y horas: [https://www.php.net/manual/es/datetime.php](https://www.php.net/manual/es/datetime.php)
- Documentación oficial de Carbon: [https://carbon.nesbot.com/docs/](https://carbon.nesbot.com/docs/)
- Ejemplo de comparación de fechas con PHP en GitHub: [https://github.com/Eminemy/Demo-PHP-Date-Compare](https://github.com/Eminemy/Demo-PHP-Date-Compare)