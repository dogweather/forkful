---
title:                "Buscando y reemplazando texto"
html_title:           "PHP: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Buscar y reemplazar texto es una tarea común en la programación que consiste en cambiar una determinada cadena de texto por otra. Los programadores lo hacen para ahorrar tiempo y automatizar procesos, ya que es mucho más rápido que cambiar manualmente cada instancia de un texto en un código extenso.

## Cómo hacerlo:
Aquí te mostramos dos ejemplos usando PHP para buscar y reemplazar texto:

```PHP
// Ejemplo 1: Reemplazar un texto en una cadena
$texto = "Hola mundo";
echo str_replace("Hola", "Adiós", $texto);
// Salida: Adiós mundo

// Ejemplo 2: Reemplazar un texto en un archivo
$archivo = "archivo.txt";
$content = file_get_contents($archivo);
$content = str_replace("Texto viejo", "Texto nuevo", $content);
file_put_contents($archivo, $content);
```

## Profundizando:
El concepto de buscar y reemplazar texto proviene de los sistemas de edición de texto de los años 60 y 70, cuando aún no existían lenguajes de programación modernos. Sin embargo, con el auge de la informática y el uso de lenguajes como PHP, esta tarea se ha vuelto aún más importante y útil en la programación.

Aunque PHP ofrece una función integrada para buscar y reemplazar texto, también existen otras opciones como expresiones regulares o el comando sed en sistemas Unix. Sin embargo, estas soluciones pueden ser más complejas y requieren un conocimiento más profundo de programación.

Para implementar una búsqueda y reemplazo eficiente, es importante tener en cuenta la sensibilidad de mayúsculas y minúsculas y buscar formas de automatizar el proceso, como en el segundo ejemplo anterior donde se utiliza la función file_put_contents para escribir el contenido modificado en el mismo archivo.

## Ver también:
- [La función str_replace en la documentación oficial de PHP](https://www.php.net/manual/es/function.str-replace.php)
- [Un tutorial sobre expresiones regulares en PHP](https://www.w3schools.com/php/php_regex.asp)