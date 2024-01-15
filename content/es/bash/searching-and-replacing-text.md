---
title:                "Buscando y reemplazando texto"
html_title:           "Bash: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Por qué?

En programación, a menudo es necesario realizar cambios en el texto de un archivo o documento. El uso de Bash para buscar y reemplazar texto en diferentes archivos puede facilitar y acelerar este proceso.

## Cómo hacerlo

Para buscar y reemplazar texto en Bash, podemos utilizar el comando `sed`. Veamos un ejemplo de cómo buscar y reemplazar la palabra "hola" por "saludos" en un archivo llamado "archivo.txt":

```Bash
sed -i 's/hola/saludos/g' archivo.txt
```

En este comando, el flag `-i` indica que queremos realizar los cambios directamente en el archivo y no solo en la salida del comando. La secuencia `s/hola/saludos/g` indica que queremos buscar la palabra "hola" y reemplazarla con "saludos", y la `g` al final le dice a Bash que lo haga en todas las coincidencias del archivo. 

El resultado será que todas las instancias de "hola" en el archivo "archivo.txt" serán reemplazadas por "saludos". Podemos usar esta misma sintaxis para buscar y reemplazar cualquier otro texto en un archivo.

## Profundizando

El comando `sed` también permite utilizar expresiones regulares para realizar búsquedas y reemplazos más complejos. Por ejemplo, podemos utilizar una expresión regular para buscar y reemplazar todas las direcciones de correo electrónico en un archivo con una etiqueta HTML para que se muestren como enlaces.

```Bash
sed -i 's/\(\w\+\@\w\+\.\w\+\)/<a href="mailto:\1">\1<\/a>/g' archivo.html
```

En este caso, la secuencia `\(\w\+\@\w\+\.\w+\)` busca cualquier cadena de texto que tenga el formato de una dirección de correo electrónico. Luego, utilizamos la etiqueta `<a>` de HTML para crear un enlace a la dirección de correo electrónico, y en la parte de reemplazo utilizamos `\1` que indica que queremos reemplazar con lo que encontramos en la búsqueda.

## Ver también

- [Documentación oficial de Bash](https://www.gnu.org/software/bash/manual/)
- [Tutorial de Bash en Español](https://www.programaenlinea.net/tutorial-bash-espanol)
- [Ejemplos de expresiones regulares en Bash](https://www.ghacks.net/2009/04/14/10-bash-regular-expression-examples-you-should-know/)