---
title:                "Obteniendo la fecha actual"
html_title:           "PHP: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

# ¿Por qué obtener la fecha actual en PHP?

Muchas veces, en la programación, necesitamos obtener la fecha actual para realizar diversas tareas. Ya sea para mostrar la fecha en un formato específico, para realizar operaciones con fechas o simplemente para tener un registro del momento en que se realizó una acción.

En PHP, obtener la fecha actual es muy sencillo y con un solo comando podemos obtenerla en el formato que necesitemos. En esta guía, vamos a explicar cómo obtener la fecha actual en PHP y algunas opciones útiles para manipularla.

## Cómo hacerlo

Para obtener la fecha actual en PHP, utilizamos la función `date()` con el formato de fecha que queramos en su parámetro. Por ejemplo, si queremos mostrar la fecha en el formato día/mes/año, utilizaremos `date('d/m/Y')`:

```PHP
<?php
echo "La fecha actual es: " . date('d/m/Y');
?>

// Output:
// La fecha actual es: 14/12/2020
```

Podemos utilizar diversas letras en el formato para obtener diferentes resultados, aquí algunos ejemplos:

- `d` - día del mes con ceros iniciales (01 a 31)
- `m` - número del mes con ceros iniciales (01 a 12)
- `Y` - año con 4 dígitos (2020)
- `y` - año con 2 dígitos (20)
- `H` - horas en formato de 24 horas (00 a 23)
- `i` - minutos con ceros iniciales (00 a 59)
- `s` - segundos con ceros iniciales (00 a 59)

También podemos agregar texto o caracteres especiales entre comillas simples para que sea parte del resultado. Por ejemplo:

```PHP
<?php
echo "Hoy es " . date('d \d\e F \d\e\l Y');
?>

// Output:
// Hoy es 14 de diciembre del 2020
```

## Profundizando en la obtención de la fecha actual

Además de la función `date()`, PHP también nos ofrece otras opciones para obtener la fecha actual con más detalles. A continuación, mencionaremos dos de estas opciones:

### time()

La función `time()` retorna la fecha actual en formato de marcas de tiempo (timestamp), que cuenta los segundos desde la medianoche del 1 de enero de 1970. Es útil para realizar operaciones con fechas o comparar fechas:

```PHP
<?php
echo "La fecha actual es: " . time();
?>

// Output:
// La fecha actual es: 1607923251
```

### DateTime

La clase `DateTime` nos permite obtener la fecha actual con diferentes formatos y también nos ofrece métodos para manipular fechas y realizar operaciones con ellas. Por ejemplo:

```PHP
<?php
$fechaActual = new DateTime();

echo "La fecha actual es: " . $fechaActual->format('d/m/Y');
?>

// Output:
// La fecha actual es: 14/12/2020
```

Con `DateTime`, también podemos obtener la fecha y hora de una zona horaria específica y hacer operaciones con diferentes fechas:

```PHP
<?php
$fechaActual = new DateTime('now', new DateTimeZone('Asia/Tokyo'));

echo "La fecha y hora actual en Tokyo es: " . $fechaActual->format('d/m/Y H:i');
?>

// Output:
// La fecha y hora actual en Tokyo es: 15/12/2020 06:54
```

## Ver también

A continuación, algunos recursos relacionados que pueden ser de ayuda:

- [Documentación oficial de PHP sobre date()](https://www.php.net/manual/es/function.date.php)
- [Documentación oficial de PHP sobre time()](https://www.php.net/manual/es/function.time.php)
- [Documentación oficial de PHP sobre la clase DateTime](https://www.php.net/manual/es/class.datetime.php)