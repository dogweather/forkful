---
title:                "PHP: Capitalizar un string"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has necesitado convertir una cadena en mayúsculas para mejorar la legibilidad de tu código? Saber cómo capitalizar una cadena es una habilidad esencial en la programación de PHP y en este artículo, aprenderás cómo hacerlo de manera sencilla y eficiente.

## Cómo hacerlo

Para capitalizar una cadena en PHP, se utiliza la función `strtoupper()` que convierte todos los caracteres de una cadena a mayúsculas. Veamos un ejemplo de cómo se implementaría esto en código:

```PHP
$cadena = "hola mundo";
echo strtoupper($cadena);
```

La salida de este código sería:

```
HOLA MUNDO
```

Sin embargo, si solo queremos capitalizar la primera letra de la cadena y mantener el resto en minúsculas, podemos utilizar la función `ucfirst()` de la siguiente manera:

```PHP
$cadena = "hola mundo";
echo ucfirst($cadena);
```

La salida sería:

```
Hola mundo
```

También es posible capitalizar cada palabra en una cadena utilizando la función `ucwords()`, que convierte la primera letra de cada palabra a mayúscula. Aquí hay un ejemplo:

```PHP
$cadena = "hola mundo";
echo ucwords($cadena);
```

La salida sería:

```
Hola Mundo
```

## Profundizando

Ahora que sabemos cómo capitalizar una cadena en PHP, es importante entender cómo esta función no solo cambia la apariencia de nuestro código, sino que también puede tener un impacto en la lógica del mismo. Por ejemplo, si tenemos una cadena que contiene nombres de países y queremos compararla con otra cadena que también contiene nombres de países pero con diferentes capitalizaciones, el resultado puede ser inesperado.

Además, es importante tener en cuenta que estas funciones solo afectan a los caracteres que están reconocidos por el conjunto de caracteres ASCII. Si tienes una cadena que contiene caracteres especiales o letras acentuadas, puede que no se capitalicen correctamente. En estos casos, es necesario utilizar la función `mb_strtoupper()` o `mb_ucfirst()` que permiten trabajar con diferentes conjuntos de caracteres.

## Ver también

- Documentación oficial de `strtoupper()`: https://www.php.net/manual/es/function.strtoupper.php
- Documentación oficial de `ucfirst()`: https://www.php.net/manual/es/function.ucfirst.php
- Documentación oficial de `ucwords()`: https://www.php.net/manual/es/function.ucwords.php
- Documentación oficial de `mb_strtoupper()`: https://www.php.net/manual/es/function.mb-strtoupper.php
- Documentación oficial de `mb_ucfirst()`: https://www.php.net/manual/es/function.mb-ucfirst.php