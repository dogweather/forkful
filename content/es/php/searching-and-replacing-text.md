---
title:                "Buscando y reemplazando texto"
date:                  2024-01-20T17:58:25.229043-07:00
model:                 gpt-4-1106-preview
simple_title:         "Buscando y reemplazando texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Buscar y reemplazar texto es básicamente cuando encuentras una cadena de texto y la cambias por otra. Los programadores lo hacen para actualizar datos, corregir errores, o modificar múltiples partes de un código rápidamente.

## Cómo:

Para buscar y reemplazar en PHP, utilizas `str_replace`. Aquí va un ejemplo simple:

```PHP
<?php
$textoOriginal = "Hola mundo crueles!";
$textoReemplazado = str_replace("crueles", "felices", $textoOriginal);
echo $textoReemplazado; // "Hola mundo felices!"
?>
```

Y si quisiéramos ser más sofisticados, podríamos usar expresiones regulares con `preg_replace`:

```PHP
<?php
$textoOriginal = "PHP es genial en 2023, muy genial!";
$textoReemplazado = preg_replace('/genial/', 'increíble', $textoOriginal, 1); // limite de reemplazo a 1
echo $textoReemplazado; // "PHP es increíble en 2023, muy genial!"
?>
```

## Deep Dive

Historicamente PHP ha ofrecido funciones para manejar textos y con el paso del tiempo, han mejorado en eficiencia y funcionalidad. Mientras `str_replace` es para reemplazos simples y directos, `preg_replace` permite patrones complejos mediante expresiones regulares, lo que da mucho poder si sabes cómo usarlas.

Alternativas como `strtr` o funciones para trabajar con arrays como `array_walk` también pueden utilizarse para manipular textos en ciertos contextos. En cuanto a implementación, recuerda que `preg_replace` puede ser más lento que `str_replace` debido a la complejidad de las expresiones regulares.

## See Also

Aquí algunos enlaces útiles para ampliar tu conocimiento:

- PHP Manual on `str_replace`: https://www.php.net/manual/es/function.str-replace.php
- PHP Manual on `preg_replace`: https://www.php.net/manual/es/function.preg-replace.php
- Expresiones Regulares 101 para probar tus regex online: https://regex101.com/
- W3Schools PHP String Functions: https://www.w3schools.com/php/php_ref_string.asp
