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

## ¿Por qué?

Convertir una cadena de texto a minúsculas puede ser útil en situaciones como el manejo de datos sensibles a mayúsculas y minúsculas, la comparación de cadenas de texto o simplemente por motivos estéticos.

## Cómo hacerlo

El proceso de conversión de una cadena de texto a minúsculas en PHP es muy sencillo gracias a la función `strtolower()`. Veamos un ejemplo práctico:

```PHP
<?php
$cadena = "ESTE TEXTO ESTÁ EN MAYÚSCULAS";
echo strtolower($cadena);
```
El código anterior producirá la siguiente salida:

```PHP
este texto está en mayúsculas
```

Como se puede ver, la función `strtolower()` convierte todas las letras de la cadena a minúsculas. Incluso si la cadena ya estaba en minúsculas, no habrá cambios en la salida.

Otra función útil para convertir una cadena a minúsculas es `mb_strtolower()`, que maneja correctamente caracteres multibyte. Esto es especialmente útil en casos donde se están manejando cadenas en idiomas diferentes al inglés.

```PHP
<?php
$cadena = "Esta es una cadena con caracteres multibyte ÁÑÕú";
echo mb_strtolower($cadena, "UTF-8");
```

La salida de este código será:

```PHP
esta es una cadena con caracteres multibyte áñõú
```

## Deep Dive

En PHP, el proceso de conversión a minúsculas se basa en las reglas del conjunto de caracteres utilizado. Por defecto, se utiliza el conjunto ISO-8859-1, que es adecuado para el inglés y otros idiomas europeos occidentales.

Sin embargo, en la práctica, es importante tener en cuenta el conjunto de caracteres al trabajar con cadenas de texto, especialmente si se manejan diferentes idiomas. En estos casos, es recomendable utilizar la función `mb_strtolower()` y especificar el conjunto de caracteres adecuado para el idioma en el que se está trabajando.

En general, es importante considerar el uso de mayúsculas y minúsculas al comparar cadenas de texto en PHP. Algunas funciones, como `strcmp()` o `strcasecmp()`, son sensibles a mayúsculas y minúsculas, mientras que otras, como `strcoll()` o `strncasecmp()`, no lo son.

Para una comprensión más profunda de cómo funcionan estas comparaciones de cadenas, se recomienda consultar la documentación oficial de PHP.

## Ver también

- [Documentación oficial de PHP sobre strtolower()](https://www.php.net/manual/en/function.strtolower.php)
- [Documentación oficial de PHP sobre mb_strtolower()](https://www.php.net/manual/en/function.mb-strtolower.php)
- [Documentación oficial de PHP sobre comparación de cadenas de texto](https://www.php.net/manual/en/book.strings.php)