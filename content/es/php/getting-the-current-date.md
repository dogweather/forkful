---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Obtener la fecha actual en PHP es un proceso para capturar la fecha y hora en este preciso momento. Esto es útil cuando necesitamos rastrear eventos en tiempo real, registrar marcas de tiempo o simplemente mostrar la fecha actual.

## Cómo hacerlo:

Aquí están los códigos importantes.

```PHP
<?php
// Define la zona horaria por defecto
date_default_timezone_set('America/Mexico_City'); 

// Muestra la fecha actual
echo date("Y-m-d H:i:s");
?>
```

El resultado seria este:

```PHP
2022-03-05 22:15:35
```

## Inmersión Profunda

Hablemos más profundamente sobre obtener la fecha actual en PHP.  

**Contexto Histórico:** PHP introdujo la función `date()` en su versión 4, y a lo largo de los años se ha convertido en una de las funciones más utilizadas para manejar fechas y horas. 

**Alternativas:** Si se necesita un objeto DateTime en lugar de una cadena, la función `new DateTime()` puede ser de utilidad. Ejemplo:

```PHP
<?php
$fecha = new DateTime();
echo $fecha->format('Y-m-d H:i:s');
?>
```

**Detalles de Implementación:** Aquí utilizamos la función `date_default_timezone_set()` para definir la zona horaria. Si no se establece una zona horaria, PHP utiliza por defecto la zona horaria del servidor, lo que puede llevar a resultados incorrectos si se está en una ubicación diferente.

## Ver También

Para obtener más detalles y posibilidades con las funciones de tiempo y fecha en PHP, visite los siguientes recursos:

- Manual de PHP - Función Date: [https://www.php.net/manual/es/function.date.php](https://www.php.net/manual/es/function.date.php)
  
- Manual de PHP - Función DateTime: [https://www.php.net/manual/es/class.datetime.php](https://www.php.net/manual/es/class.datetime.php)