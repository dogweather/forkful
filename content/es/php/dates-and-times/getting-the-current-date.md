---
title:                "Obteniendo la fecha actual"
date:                  2024-02-03T19:10:15.616698-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obteniendo la fecha actual"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Obtener la fecha actual en PHP es una tarea fundamental que te permite recuperar y manipular la fecha y la hora del sistema. Esto es crucial para funciones como el registro de actividades, marcar temporalmente publicaciones, programar eventos o realizar operaciones sensibles al tiempo en tus aplicaciones.

## Cómo hacerlo:
### PHP Nativo
La función incorporada `date()` de PHP es la manera más directa de obtener la fecha actual. Puedes formatear la fecha de diversas maneras especificando el parámetro de formato.

```php
echo date("Y-m-d"); // Muestra: 2023-04-01 (por ejemplo)
echo date("l, F j, Y"); // Muestra: Saturday, April 1, 2023
```

Para obtener la fecha y hora con soporte de zona horaria, puedes utilizar la clase `DateTime` junto con `DateTimeZone`.

```php
$dateTime = new DateTime('now', new DateTimeZone('America/New_York'));
echo $dateTime->format('Y-m-d H:i:s'); // Muestra: 2023-04-01 12:00:00 (por ejemplo)
```

### Usando Carbon (Una Biblioteca de Terceros Muy Popular)
[Carbon](https://carbon.nesbot.com/) es una extensión simple de la API para `DateTime` que proporciona una manera más limpia y fluida de trabajar con fechas y horas.

Primero, asegúrate de tener Carbon instalado a través de Composer:
```bash
composer require nesbot/carbon
```

Luego, puedes usarlo para obtener la fecha actual:

```php
use Carbon\Carbon;

echo Carbon::now(); // Muestra: 2023-04-01 12:00:00 (por ejemplo, en el formato por defecto)
echo Carbon::now()->toDateString(); // Muestra: 2023-04-01
echo Carbon::now()->format('l, F j, Y'); // Muestra: Saturday, April 1, 2023
```

Carbon enriquece el manejo de fechas y horas en PHP añadiendo legibilidad y una abundancia de funcionalidades para la manipulación, comparación y formateo del tiempo.
