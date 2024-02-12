---
title:                "Escribiendo en el error estándar"
aliases: - /es/php/writing-to-standard-error.md
date:                  2024-02-03T19:33:58.898930-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escribiendo en el error estándar"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?

Escribir en el error estándar (stderr) en PHP se trata de dirigir mensajes de error o diagnósticos de forma separada de la salida estándar (stdout), permitiendo a los desarrolladores gestionar mejor sus flujos de salida para la depuración y el registro de actividades. Los programadores utilizan esta técnica para asegurarse de que los mensajes de error no interfieran con la salida del programa, facilitando el monitoreo y la resolución de problemas en las aplicaciones.

## Cómo hacerlo:

En PHP, escribir en stderr se puede lograr utilizando la función `fwrite()` junto con la constante predefinida `STDERR`, que representa el flujo de salida de errores.

```php
<?php
// Escribiendo un mensaje simple en stderr.
fwrite(STDERR, "Este es un mensaje de error.\n");
```

Salida de muestra cuando se ejecuta el script desde la línea de comandos:
```
Este es un mensaje de error.
```

Para demostrar un uso más práctico, considera un escenario donde estás analizando la entrada del usuario y encuentras datos inesperados:
```php
<?php
$input = 'datos inesperados';

// Simulando un error al procesar la entrada del usuario.
if ($input === 'datos inesperados') {
    fwrite(STDERR, "Error: Entrada inesperada recibida.\n");
    exit(1); // Saliendo con un valor no cero para indicar un error.
}
```

Aunque las capacidades integradas de PHP para manejar stderr son generalmente suficientes, cuando se trata de aplicaciones más complejas o se desea integrar el registro de stderr con sistemas externos, bibliotecas de terceros como Monolog pueden ser un poderoso aliado. Monolog es una biblioteca de registro que puede manejar stderr entre muchos otros destinos (archivos, sockets, etc.).

Usando Monolog para escribir en stderr:

Primero, asegúrate de tener Monolog instalado a través de Composer:
```
composer require monolog/monolog
```

Luego, puedes configurar Monolog para usar el `StreamHandler` dirigido a `php://stderr`:

```php
<?php
require 'vendor/autoload.php';

use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Crear un canal de registro
$log = new Logger('nombre');
$log->pushHandler(new StreamHandler('php://stderr', Logger::WARNING));

// Agregar un mensaje de registro en stderr
$log->warning('Este es un mensaje de advertencia.');
```

El código anterior utiliza Monolog para enviar un mensaje de advertencia a stderr, lo que es particularmente útil para aplicaciones que requieren configuraciones de registro detalladas o monitoreo de registros externos.
