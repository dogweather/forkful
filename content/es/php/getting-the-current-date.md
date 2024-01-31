---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:15:55.742396-07:00
html_title:           "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Obtener la fecha actual en PHP es básicamente saber qué día es hoy según el servidor donde resides. Como programadores lo hacemos por miles de razones: desde mostrar la fecha en una página hasta calcular tiempos de expiración para sesiones o ofertas.

## Cómo hacerlo:

Para obtener simplemente la fecha de hoy en PHP, usas la función `date()`. Aquí tienes un ejemplo:

```PHP
echo date("Y-m-d"); // Formato Año-Mes-Día
```

Salida de ejemplo:
```
2023-04-05
```

Si necesitas más, como la fecha y hora actual con zona horaria, puedes hacerlo así:

```PHP
date_default_timezone_set('Europe/Madrid'); // Ajusta a tu zona horaria
echo date("Y-m-d H:i:s");
```

Salida de ejemplo:
```
2023-04-05 14:23:48
```

## A Fondo

Desde PHP 4, `date()` ha sido la forma estándar de obtener la fecha y hora. Antes existían otras formas más rudimentarias relacionadas con el sistema operativo del servidor. Hoy, alternativas como la clase `DateTime` ofrecen más flexibilidad y opciones orientadas a objetos:

```PHP
$fecha = new DateTime();
echo $fecha->format("Y-m-d H:i:s");
```

La implementación de `date()` y `DateTime` tiene en cuenta la zona horaria que puede ser configurada globalmente a través de `date_default_timezone_set()` o localmente por cada objeto `DateTime`. Es crucial manejar bien las zonas horarias para evitar confusiones con los tiempos.

## Ver También

- Documentación oficial de PHP sobre la función `date()`: [php.net/manual/es/function.date.php](https://www.php.net/manual/es/function.date.php)
- Uso de la clase `DateTime`: [php.net/manual/es/class.datetime.php](https://www.php.net/manual/es/class.datetime.php)
- Función `date_default_timezone_set()`: [php.net/manual/es/function.date-default-timezone-set.php](https://www.php.net/manual/es/function.date-default-timezone-set.php)
- Listado de zonas horarias soportadas: [php.net/manual/es/timezones.php](https://www.php.net/manual/es/timezones.php)
