---
title:                "Bash: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

### ¿Por qué extraer subcadenas en Bash?

Extraer subcadenas es una técnica útil en la programación Bash que permite obtener una parte específica de una cadena de texto. Esto puede ser útil en situaciones donde solo se necesita una parte de la información contenida en una cadena más larga. Por ejemplo, si se tiene una lista de nombres en un archivo y se desea obtener solo los apellidos, se podría utilizar la técnica de extracción de subcadenas para lograrlo.

### Cómo extraer subcadenas en Bash

La extracción de subcadenas en Bash se puede lograr utilizando una combinación de variables, comandos y expresiones regulares. A continuación, se presentan algunos ejemplos de código para ilustrar cómo se puede realizar esta tarea.

```Bash
# Definir una cadena de texto
texto="Hola a todos"

# Extraer la primera palabra de la cadena
primera_palabra=${texto%% *}

echo "La primera palabra es: $primera_palabra"
# Output: La primera palabra es: Hola

# Extraer la segunda palabra de la cadena
segunda_palabra=${texto##* }

echo "La segunda palabra es: $segunda_palabra"
# Output: La segunda palabra es: todos

# Extraer la tercera palabra de la cadena
tercera_palabra=${texto##* * }

echo "La tercera palabra es: $tercera_palabra"
# Output: La tercera palabra es: todos
```

En este ejemplo, el operador `%%` se utiliza para extraer todo lo que está antes del primer espacio en blanco y el operador `##` se utiliza para extraer todo lo que está después del último espacio en blanco.

### Inmersión profunda en la extracción de subcadenas

Además de los ejemplos presentados anteriormente, existen otras opciones y técnicas para extraer subcadenas en Bash. Por ejemplo, se pueden utilizar expresiones regulares más complejas para extraer cadenas según ciertos patrones o criterios. También se pueden utilizar variables especiales como `IFS` (Internal Field Separator) para definir un delimitador personalizado en la cadena.

Es importante tener en cuenta que la posición de los caracteres dentro de una cadena puede afectar el resultado de la extracción de subcadenas. Por lo tanto, es recomendable revisar cuidadosamente la cadena y los operadores utilizados antes de realizar la extracción.

## Ver también

- [Documentación de Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Expresiones regulares en Bash](https://tldp.org/LDP/abs/html/x17129.html)
- [Variables especiales en Bash](https://tldp.org/LDP/abs/html/internalvariables.html#SUBSTR-REF)