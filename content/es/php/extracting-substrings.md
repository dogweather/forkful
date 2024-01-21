---
title:                "Extracción de subcadenas"
date:                  2024-01-20T17:46:09.231889-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracción de subcadenas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué es y Por Qué?

Extraer subcadenas significa sacar trozos específicos de una cadena de texto. Programadores lo hacen para analizar datos, validar entradas, o simplemente trabajar con partes relevantes de una cadena.

## ¿Cómo se hace?

Veamos cómo cortar esa cadena a punta de código:

```php
// Ejemplo 1: Extraer con substr()
$texto = "Hola, programadores!";
$subcadena = substr($texto, 7, 13);
echo $subcadena; // Salida: programadores

// Ejemplo 2: Usar caracteres negativos
$texto = "Buenas tardes, disfruten el código";
$subcadena = substr($texto, -6, -1);
echo $subcadena; // Salida: código

// Ejemplo 3: Inicio sin longitud
$texto = "PHP es chévere, ¿no?";
$subcadena = substr($texto, 4);
echo $subcadena; // Salida: es chévere, ¿no?
```

## Inmersión Profunda

Hace años, con PHP más primitivo, cortar cadenas era un rollo. Hoy, gracias a funciones como `substr()`, la vida es más fácil. Podrías usar `mb_substr()` si te metes con UTF-8, porque otras funciones se lían con caractéres multibyte. Si quieres algo más específico, `preg_match()` y expresiones regulares son tus amigotes, pero eso ya es meterse en honduras.

Alternativas como `strpos()` y `strstr()` están disponibles para tareas más sencillas, como encontrar la posición de una subcadena o extraer todo después de un caracter específico. Recuerda, `substr()` no falla si usas índices negativos, pero otras funciones pueden hacer pucheros.

La implementación de estas funciones se basa en la idea de tratar cadenas como arrays de caracteres, permitiéndote acceder a cada elemento con facilidad. Este acceso directo es lo que hace tan versátil a PHP al manejar texto.

## Vea También

- La documentación oficial de PHP sobre `substr()`: https://www.php.net/manual/es/function.substr.php
- Un paseo por las funciones de manejo de cadenas de PHP: https://www.php.net/manual/es/ref.strings.php
- Una guía para entender las expresiones regulares en PHP: https://www.php.net/manual/es/book.pcre.php
- Sobre el manejo de caracteres multibyte: https://www.php.net/manual/es/book.mbstring.php