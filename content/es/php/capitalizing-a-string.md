---
title:                "Capitalizando una cadena de texto"
html_title:           "PHP: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Capitalizar una cadena significa convertir la primera letra de cada palabra en mayúsculas. Los programadores lo hacen para mejorar la legibilidad y la presentación de los textos en la interfaz del usuario.

## Cómo hacerlo:

Para capitalizar una cadena de texto en PHP, usamos la función `ucwords()`. Mira este ejemplo:

```PHP
<?php
$cadena = "hola mundo";
$cadena_capitalizada = ucwords($cadena);

echo $cadena_capitalizada; // Se imprimirá: "Hola Mundo"
?>
```

Como puedes observar, `ucwords()` convierte la primera letra de cada palabra a mayúscula.

## Inmersión Profunda:

Historia breve: PHP no siempre tuvo una función integrada para capitalizar cadenas. Antes de la versión 5, tenías que escribir manualmente esta función.

Alternativas: También tenemos `strtoupper()` que convierte todas las letras a mayúsculas, y `lcfirst()` que convierte la primera letra a minúscula.

Sobre el rendimiento: `ucwords()` es bastante eficiente. Pero si estás trabajando con cadenas muy grandes, ten en cuenta que puede llevar un poco de tiempo procesarlas.

## Ver También:

Para más detalles y ejemplos, echa un vistazo a los siguientes recursos:

- Documentación oficial de PHP para `ucwords()`: [https://www.php.net/manual/es/function.ucwords.php](https://www.php.net/manual/es/function.ucwords.php)
  
- Para aprender más acerca de `strtoupper()` y `lcfirst()`: [https://www.php.net/manual/es/function.strtoupper.php](https://www.php.net/manual/es/function.strtoupper.php) y [https://www.php.net/manual/es/function.lcfirst.php](https://www.php.net/manual/es/function.lcfirst.php)