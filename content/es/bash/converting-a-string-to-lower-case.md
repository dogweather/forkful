---
title:                "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una cadena a minúsculas en Bash?

Si estás escribiendo un script en Bash que maneja cadenas de texto, es posible que en algún momento necesites convertir una cadena a minúsculas. Esto puede ser útil si quieres asegurarte de que tus datos estén en el mismo formato o si quieres hacer una comparación de cadenas sin importar las mayúsculas o minúsculas. A continuación te mostraremos cómo hacerlo.

## Cómo hacerlo en Bash

Puedes usar el comando `tr` para convertir una cadena a minúsculas en Bash. Este comando se encarga de transformar o eliminar caracteres de un archivo o entrada de texto.

```Bash
# Definimos una variable con una cadena de texto
cadena="Bash es divertido!"
# Usamos el comando tr para convertirla a minúsculas
cadena=$(echo "$cadena" | tr '[:upper:]' '[:lower:]')
# Imprimimos el resultado
echo "$cadena"
```
La salida será la siguiente:
```Bash
bash es divertido!
```

Para entender mejor lo que está pasando en el código, vamos a desglosarlo.

- En la primera línea definimos una variable llamada `cadena` con una cadena de texto en mayúsculas.
- En la segunda línea utilizamos el comando `tr` junto con la función `echo` para convertir la cadena a minúsculas.
- La parte `[:upper:]` especifica los caracteres que queremos convertir, en este caso todas las mayúsculas.
- Y la parte `[:lower:]` especifica a qué queremos convertirlos, en este caso a minúsculas.
- Por último, en la tercera línea imprimimos el resultado en la consola.

## Profundizando en la conversión de cadenas a minúsculas en Bash

El comando `tr` también puede ser utilizado para realizar otras transformaciones en cadenas de texto, como eliminar caracteres o reemplazarlos por otros. Además, también se puede usar en conjunto con otros comandos y herramientas en Bash para realizar tareas más complejas.

Por ejemplo, se puede combinar `tr` con `sed` para hacer una búsqueda y reemplazo de una cadena en específico. O también se puede usar con `grep` para filtrar un archivo por un patrón de caracteres en minúsculas.

En resumen, el comando `tr` nos permite manipular cadenas de texto de diferentes maneras y puede ser muy útil cuando se trabaja con scripts en Bash.

## Ver también

- [Guía de tr en la documentación de Bash](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#tr)
- [Uso del comando tr en Linux](https://www.tecmint.com/linux-tr-commands/)
- [Ejemplos de uso de tr en Bash](https://linuxconfig.org/bash-tr-command)