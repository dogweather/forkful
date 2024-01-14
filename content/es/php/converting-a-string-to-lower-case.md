---
title:    "PHP: Convirtiendo un string a minúsculas"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué convertir una cadena a minúsculas?

Convertir una cadena de texto a minúsculas puede ser útil en diferentes situaciones de programación, como por ejemplo al validar entradas de usuarios, comparar cadenas sin importar su capitalización o formatear cadenas para mostrarlas de forma uniforme. Además, puede ser necesario para que el código sea más legible y fácil de entender para otros programadores.

## Cómo hacerlo:

```PHP
// Ejemplo de cómo convertir una cadena a minúsculas en PHP
$cadena = "ESTO ES UNA CADENA EN MAYÚSCULAS";
$cadena_minusculas = strtolower($cadena);

echo $cadena_minusculas;

//Salida: esto es una cadena en mayúsculas
```

## Un paseo en profundidad:

Para convertir una cadena a minúsculas en PHP, utilizamos la función `strtolower()`, que toma como parámetro la cadena a convertir y devuelve una nueva cadena en minúsculas. Esta función puede manejar tanto cadenas en inglés como en otros idiomas como el español.

Una cosa importante a tener en cuenta es que la función `strtolower()` no convierte todos los caracteres a minúsculas, solo aquellos que pueden ser convertidos a través del sistema de codificación de caracteres utilizado en PHP. Por lo tanto, si tienes caracteres especiales en tu cadena, quizás sea necesario utilizar otras funciones para asegurarse de que se conviertan correctamente.

## Ver también:

- [La función strtolower() en la documentación oficial de PHP](https://www.php.net/manual/es/function.strtolower.php)
- [Ejemplos de uso de strtolower() en diferentes situaciones](https://www.w3schools.com/php/func_string_strtolower.asp)
- [Otras funciones útiles para el manejo de cadenas en PHP](https://www.php.net/manual/es/ref.strings.php)