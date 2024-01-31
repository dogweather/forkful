---
title:                "Capitalizando una cadena de texto"
date:                  2024-01-19
html_title:           "Arduino: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"

category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qué y por qué?
Capitalizar una cadena significa convertir la primera letra de cada palabra a mayúscula. Los programadores lo hacen para asegurar que los nombres propios, títulos o para dar énfasis visual sigan las reglas de la gramática y mejoren la legibilidad.

## Cómo hacerlo:
Capitaliza cadenas en PHP con `ucwords` o `mb_convert_case` si necesitas soporte para UTF-8:

```PHP
<?php
$texto = "bienvenidos a mi blog de programación en php.";

// Capitalizar con ucwords:
echo ucwords($texto);
// Salida: "Bienvenidos A Mi Blog De Programación En Php."

// Si necesitas considerar caracteres UTF-8:
echo mb_convert_case($texto, MB_CASE_TITLE, "UTF-8");
// Salida: "Bienvenidos A Mi Blog De Programación En Php."
?>
```
## Inmersión Profunda
Antes, PHP no manejaba bien los caracteres no ASCII. `ucwords` y `strtoupper` podían fallar con caracteres especiales o acentuados. Desde PHP 5.4.0, `mb_convert_case` mejora el tratamiento de caracteres multibyte como los acentos en español.

Alternativas incluyen:
- `strtoupper` para convertir toda la cadena a mayúsculas.
- `strtolower` para todo en minúsculas y luego `ucfirst` para capitalizar solo la primera letra de la cadena.

Detalles de implementación:
- `ucwords` es rápido y adecuado para la mayoría de las necesidades, pero con limitaciones en cuanto a locales.
- `mb_convert_case` usa la extensión `mbstring` para soporte extendido de codificación de caracteres, incluyendo caracteres no latinos.

## Ver también
- [PHP String Functions](https://www.php.net/manual/en/ref.strings.php) - Funciones de cadenas en PHP.
- [PHP Multibyte String Functions](https://www.php.net/manual/en/ref.mbstring.php) - Funciones de la extensión mbstring en PHP.
- [PHP ucwords](https://www.php.net/manual/en/function.ucwords.php) - Documentación oficial de `ucwords`.
- [PHP mb_convert_case](https://www.php.net/manual/en/function.mb-convert-case.php) - Documentación oficial de `mb_convert_case`.
