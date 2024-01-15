---
title:                "Capitalizando una cadena"
html_title:           "PHP: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has tenido una cadena de texto que necesitaba ser escrita en mayúsculas? Dependiendo de lo que estés haciendo en tu código, puede ser necesario capitalizar una cadena para que se muestre correctamente en pantalla. Afortunadamente, con PHP, esto es muy fácil de lograr.

## Cómo hacerlo

Para capitalizar una cadena en PHP, puedes usar la función `strtoupper()`. Por ejemplo:

```PHP
$cadena = "hola mundo";
echo strtoupper($cadena);
```

La salida sería:

```PHP
HOLA MUNDO
```

También puedes usar la función `mb_strtoupper()` si necesitas manejar caracteres multibyte, como los acentos en español. Por ejemplo:

```PHP
$cadena = "hola mundo";
echo mb_strtoupper($cadena, 'UTF-8');
```

La salida sería la misma que antes: `HOLA MUNDO`.

## Profundizando

Ahora que sabes cómo capitalizar una cadena en PHP, es importante entender que esta función no solo afecta a las letras minúsculas. También cambia los números y otros caracteres, como los símbolos de puntuación, a su versión en mayúsculas. Por ejemplo, si tienes una cadena que dice "hoy es 25 de diciembre", al capitalizarla con `strtoupper()` el resultado sería "HOY ES 25 DE DICIEMBRE".

También es importante tener en cuenta que la función `strtoupper()` solo convierte las letras a su versión en mayúsculas, pero no elimina los acentos. Si necesitas que los acentos sean removidos durante la conversión, puedes usar la función `iconv()` en conjunto con `strtoupper()`. Por ejemplo:

```PHP
$cadena = "hola mundo éste es un texto";
echo strtoupper(iconv('UTF-8', 'ASCII//TRANSLIT', $cadena));
```

La salida sería:

```PHP
HOLA MUNDO ESTE ES UN TEXTO
```

## Ver también

- Documentación oficial sobre la función `strtoupper()`: https://www.php.net/manual/es/function.strtoupper.php
- Documentación oficial sobre la función `mb_strtoupper()`: https://www.php.net/manual/es/function.mb-strtoupper.php
- Documentación oficial sobre la función `iconv()`: https://www.php.net/manual/es/function.iconv.php