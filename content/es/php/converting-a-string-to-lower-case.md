---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "PHP: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

¿Qué & Por qué?
Converting a string to lower case, or in Spanish, convertir una cadena a minúsculas, es un proceso común en la programación donde se cambian todas las letras de una cadena de texto a su versión en minúsculas. Los programadores lo hacen para facilitar la comparación de cadenas de texto y para asegurarse de que los datos ingresados por el usuario sean consistentes.

## Cómo hacerlo:
Para convertir una cadena a minúsculas en PHP, se puede utilizar la función `strtolower()`. Esta función toma una cadena como parámetro y devuelve la misma cadena con todas las letras en minúsculas.

Ejemplo de código:
```PHP
$cadena = "¡Hola Mundo!";
echo strtolower($cadena);
```

Resultado:
```
¡hola mundo!
```

También se puede utilizar la función `mb_strtolower()` para convertir una cadena con caracteres multibyte a minúsculas. Esta función es útil para trabajar con cadenas en otros idiomas que contengan caracteres especiales.

Ejemplo de código:
```PHP
$cadena = "Álvaro";
echo mb_strtolower($cadena);
```

Resultado:
```
álvaro
```

## Detalles en profundidad:
Converting a string to lower case puede remontarse a los primeros sistemas informáticos que utilizaban codificación ASCII, donde no había diferenciación entre mayúsculas y minúsculas. Sin embargo, con la evolución de los lenguajes de programación y la inclusión de caracteres especiales, se hizo necesario tener una forma de convertir las cadenas a un formato estándar.

Además de las funciones mencionadas anteriormente, también se pueden utilizar expresiones regulares o métodos manuales para convertir cadenas a minúsculas en PHP. Sin embargo, es importante tener en cuenta que el uso de estas alternativas puede ser menos eficiente y menos confiable que las funciones incorporadas de PHP.

Para implementar la conversión de cadenas a minúsculas en un programa, es importante tener en cuenta el conjunto de caracteres que se está utilizando. Si se trabaja con idiomas que utilizan caracteres multibyte, se debe utilizar la función `mb_strtolower()` para garantizar que todos los caracteres se conviertan correctamente.

## Ver también:
- [Función strtolower() en la documentación de PHP](https://www.php.net/manual/es/function.strtolower.php)
- [Función mb_strtolower() en la documentación de PHP](https://www.php.net/manual/es/function.mb-strtolower.php)
- [Expresiones regulares en PHP](https://www.w3schools.com/php/php_regex.asp)