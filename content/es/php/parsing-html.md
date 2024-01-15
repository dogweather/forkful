---
title:                "Analizando html"
html_title:           "PHP: Analizando html"
simple_title:         "Analizando html"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/parsing-html.md"
---

{{< edit_this_page >}}

## Por qué
Si eres un desarrollador de PHP, probablemente hayas oído hablar de la técnica de "parsear" HTML. Pero, ¿por qué alguien querría hacer eso? La respuesta es simple: para extraer datos de una página web de forma automatizada.

Imagina que tienes un sitio web que recopila información de diferentes fuentes. Seria tedioso tener que copiar y pegar manualmente los datos de cada página. En su lugar, puedes utilizar un programa que haga el trabajo por ti, parseando el HTML y extrayendo los datos necesarios.

## Cómo hacerlo
La función principal para parsear HTML en PHP es `file_get_contents()`. Esta función recupera el contenido completo de una URL y lo almacena en una variable. A continuación, puedes utilizar la función `preg_match()` para buscar patrones específicos en el contenido y extraer los datos que necesitas.

Veamos un ejemplo simple de cómo extraer el título de una página web utilizando estas funciones:

```
<?php
// Obtener el contenido de la página
$html = file_get_contents('https://www.ejemplo.com/');
// Buscar el patrón del título
preg_match('@<title>(.*)</title>@', $html, $title);
// Imprimir el título
echo "El título de la página es: " . $title[1];
?>
```

En este caso, hemos utilizado expresiones regulares para buscar el texto que se encuentra entre las etiquetas `<title>` de la página web.

## Profundizando
Aunque este es solo un ejemplo básico, la técnica de parsear HTML puede ser utilizada para extraer cualquier tipo de datos de una página web. Puedes utilizar funciones como `preg_match_all()` para extraer múltiples ocurrencias de un patrón, o incluso crear tus propias clases para manejar el proceso de parsing de manera más eficiente.

También es importante tener en cuenta que el parsing de HTML puede ser algo complicado, ya que el contenido de una página web puede variar mucho de una a otra. Por lo tanto, es recomendable tener un buen conocimiento de expresiones regulares y del formato en que se presenta el HTML para obtener resultados precisos.

## Ver también
- [Documentación oficial de PHP sobre la función `file_get_contents()`](https://www.php.net/manual/es/function.file-get-contents.php)
- [Documentación oficial de PHP sobre la función `preg_match()`](https://www.php.net/manual/es/function.preg-match.php)
- [Expresiones regulares en PHP](https://www.php.net/manual/es/book.pcre.php)