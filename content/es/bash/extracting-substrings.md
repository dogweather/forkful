---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Extraer subcadenas es el proceso de obtener partes pequeñas, específicas de una cadena más grande. Los programadores lo hacen para manipular, analizar o transformar datos en formatos más útiles.

## ¿Cómo se hace?

Primero, vamos a establecer un string:

```Bash
cadena="Bienvenidos a Bash, amigos programadores."
```
Para extraer una subcadena, usamos la siguiente sintaxis:

```Bash
echo ${cadena:10:6}
```
Esto extraerá 6 caracteres a partir del índice 10. La salida será:

```Bash
"a Bash"
```
### Deep Dive

En sus primeras versiones, el interprete Bash no tenía capacidad para extraer subcadenas directamente. Con el tiempo, esta funcionalidad fue introducida para facilitar las operaciones de cadena, y ahora es uno de los principales usos de Bash.

Existen alternativas a Bash para extraer subcadenas, como AWK y Perl. Sin embargo, Bash es muy utilizado y práctico, especialmente para los usuarios de Linux y MacOS.

Detrás de escena, cuando extraes una subcadena, Bash la recorre desde el índice que especificaste hasta la longitud que definiste. Si este índice y longitud caen dentro de los límites de la cadena, se devuelve la subcadena. En caso contrario, se devolverá una cadena vacía.

## Ver También

1. [Tutorial de Bash](https://www.learnshell.org/es/Bash_Scripting)
2. [Bash en Wikipedia](https://es.wikipedia.org/wiki/Bash)
3. [Extraer subcadenas en Bash](http://tldp.org/LDP/abs/html/string-manipulation.html)

Lo más importante, practica y experimenta por ti mismo. ¡Buena codificación!