---
title:                "Buscando y reemplazando texto"
html_title:           "Fish Shell: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# ¿Qué y por qué?

Buscando y reemplazando texto es una tarea común en la programación. Consiste en encontrar una cadena específica de texto dentro de un archivo o en un directorio y reemplazarlo por otro. Los programadores lo hacen para hacer cambios rápidos y masivos en su código o para actualizar información en sus programas.

# ¡Cómo hacerlo!

Al igual que con muchos otros lenguajes de programación, en Fish Shell también podemos buscar y reemplazar texto. Utilizando el comando `sed`, podemos especificar la cadena de texto que queremos reemplazar y el texto de reemplazo. Por ejemplo:

```
Fish Shell> sed 's/hola/adiós/' archivo.txt
```

Este comando reemplazará todas las instancias de "hola" en el archivo "archivo.txt" por "adiós". También podemos usar expresiones regulares para buscar patrones más complejos y reemplazarlos. Por ejemplo:

```
Fish Shell> sed 's/[0-9]\{4\}/####/' archivo.txt
```

Este comando buscará cualquier año de cuatro dígitos y lo reemplazará por "####". Otro comando útil para buscar y reemplazar texto es `grep`, que nos permitirá buscar una cadena específica en un archivo o directorio. Por ejemplo:

```
Fish Shell> grep "keyword" archivo.txt
```

Este comando nos mostrará todas las líneas en el archivo "archivo.txt" que contengan la palabra "keyword". Para reemplazarlo, podemos usar el comando `xargs` junto con `sed`. Por ejemplo:

```
Fish Shell> grep "keyword" archivo.txt | xargs sed 's/keyword/nueva palabra/'
```

Este comando buscará la palabra "keyword" en el archivo "archivo.txt" y la reemplazará por "nueva palabra". Una ventaja de utilizar `xargs` es que también podemos utilizar la salida de otros comandos como entrada para `sed`.

# Inmersión profunda

La búsqueda y reemplazo de texto ha sido una tarea esencial en la programación desde los primeros días. En la década de 1970, el comando `sed` fue creado para hacer este proceso más eficiente y escalable. Otras alternativas populares incluyen `awk`, `perl` y `python`. Fish Shell ha integrado estos comandos en su sintaxis para hacerlo más accesible para los programadores.

# Véase también

- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guía de comandos de Fish Shell](https://fishshell.com/docs/current/commands.html)