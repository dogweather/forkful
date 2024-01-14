---
title:                "Bash: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

A veces, en programación, puede ser necesario eliminar ciertos caracteres que cumplen con un patrón en un texto o cadena de caracteres. Esto puede ser útil para limpiar datos o para fines de validación. En este artículo, aprenderemos cómo eliminar caracteres que coinciden con un patrón en Bash.

## Cómo hacerlo

Para eliminar caracteres que cumplen con un patrón en Bash, podemos utilizar el comando `sed` (stream editor). Este comando nos permite reemplazar caracteres o patrones en un archivo o en la salida de un comando. Veamos un ejemplo de cómo utilizarlo:

```
Bash ...
sed 's/a//' texto.txt
```

En este ejemplo, estamos utilizando `sed` para eliminar todas las letras "a" en el archivo de texto `texto.txt`. El resultado de esto sería un archivo modificado en el que las letras "a" han sido eliminadas.

Otro ejemplo podría ser eliminar todos los números de un archivo de texto:

```
Bash ...
sed 's/[0-9]//' texto.txt
```

En este caso, estamos utilizando una expresión regular entre corchetes para indicar que queremos eliminar todos los números del archivo `texto.txt`.

## Profundizando

Para comprender mejor cómo funciona `sed` y cómo podemos utilizar expresiones regulares para eliminar caracteres que cumplen con ciertos patrones, es importante familiarizarnos con el concepto de expresiones regulares. Estas son secuencias de caracteres que nos permiten buscar patrones específicos en una cadena de texto. Aprender a utilizar expresiones regulares es fundamental para trabajar con `sed` y para realizar tareas más avanzadas de manipulación de texto.

Una expresión regular básica consta de uno o más caracteres que coinciden directamente con la cadena que estás buscando. Por ejemplo, si queremos encontrar la palabra "hola" en un texto, podríamos usar la expresión regular `hola`. Sin embargo, expresiones regulares más avanzadas nos permiten indicar patrones más complejos, como búsqueda de palabras que comienzan con una letra específica o contienen ciertos caracteres.

Es importante tener en cuenta que el uso de expresiones regulares puede variar ligeramente dependiendo del sistema operativo y la versión de `sed` que estés utilizando. Por lo tanto, es recomendable consultar la documentación específica de tu sistema antes de utilizar expresiones regulares con `sed`.

## Ver también

- [Documentación de `sed`](https://www.gnu.org/software/sed/manual/sed.html)
- [Introducción a expresiones regulares](https://docs.microsoft.com/es-es/dotnet/standard/base-types/regular-expression-language-quick-reference) (en español)
- [Tutorial de expresiones regulares en Bash](https://www.wallstreetmojo.com/regular-expression-in-bash/) (en inglés)