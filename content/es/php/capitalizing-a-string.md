---
title:    "PHP: Capitalizando una cadena"
keywords: ["PHP"]
---

{{< edit_this_page >}}

¿Por qué capitalizar una cadena en PHP?

A menudo, en la programación, es necesario manejar cadenas de caracteres en mayúsculas o minúsculas, ya sea para cumplir con ciertas convenciones o para facilitar la comparación entre cadenas. Al capitalizar una cadena, podemos asegurarnos de que todas las palabras comiencen con una letra mayúscula, lo que puede ser útil para fines estéticos o de funcionalidad.

## Cómo hacerlo

Para capitalizar una cadena en PHP, podemos utilizar la función `ucwords()`, la cual convierte a mayúsculas el primer carácter de cada palabra en una cadena. Podemos pasar como argumento la cadena que queremos capitalizar y, opcionalmente, un segundo parámetro que indique los caracteres que deben ser considerados como separadores de palabras.

```PHP
$string = "hola mundo";
$capitalized_string = ucwords($string);
echo $capitalized_string; // Salida: Hola Mundo

$string2 = "bienvenidos a mi blog";
$capitalized_string2 = ucwords($string2, " "); // Usamos espacio como separador de palabras
echo $capitalized_string2; // Salida: Bienvenidos A Mi Blog
```

## Profundizando

La función `ucwords()` utiliza como referencia para capitalizar el primer carácter de cada palabra el valor de la variable `localculture` del sistema operativo. Esto significa que los resultados pueden variar según el idioma del sistema. En algunos casos, puede ser necesario utilizar la función `mb_convert_case()` de la extensión de multibyte string para un mejor manejo de caracteres especiales.

### Ejemplo con `mb_convert_case()`
```PHP
$string = "día feliz";
$capitalized_string = mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
echo $capitalized_string; // Salida: Día Feliz
```

En resumen, la función `ucwords()` es una forma conveniente y sencilla de capitalizar una cadena en PHP, pero es importante tener en cuenta su comportamiento según el idioma del sistema.

## Véase también

- [Documentación oficial de ucwords() en PHP.net](https://www.php.net/manual/es/function.ucwords.php)
- [Documentación oficial de mb_convert_case() en PHP.net](https://www.php.net/manual/es/function.mb-convert-case.php)