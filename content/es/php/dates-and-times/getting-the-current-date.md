---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:15.616698-07:00
description: "C\xF3mo hacerlo: La funci\xF3n incorporada `date()` de PHP es la manera\
  \ m\xE1s directa de obtener la fecha actual. Puedes formatear la fecha de diversas\
  \ maneras\u2026"
lastmod: '2024-03-13T22:44:59.171141-06:00'
model: gpt-4-0125-preview
summary: "La funci\xF3n incorporada `date()` de PHP es la manera m\xE1s directa de\
  \ obtener la fecha actual."
title: Obteniendo la fecha actual
weight: 29
---

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
