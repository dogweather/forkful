---
title:                "Bash: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Una de las acciones más comunes en la programación es capitalizar una cadena de texto. Puede ser necesario por diversas razones, como por ejemplo estética, para cumplir con ciertos requisitos de formato o simplemente para facilitar la lectura del texto.

## Cómo hacerlo

Para capitalizar una cadena en Bash, podemos hacer uso de una sola línea de código utilizando el comando `tr` (traducir). Por ejemplo:

```Bash
cadena="hola mundo"
echo $cadena | tr '[a-z]' '[A-Z]'
```

Esto producirá la salida `HOLA MUNDO`, lo que significa que la función `tr` ha convertido todas las letras minúsculas en mayúsculas.

Para explicar un poco más, el comando `tr` toma dos argumentos: el primero es el conjunto de caracteres que queremos reemplazar, y el segundo es el conjunto de caracteres en los que queremos reemplazarlos. En este caso, hemos definido el primer argumento como todas las letras minúsculas de la "a" a la "z" y el segundo argumento como las mismas letras en mayúsculas.

Otra forma de capitalizar una cadena es utilizando el comando `awk`, que es una herramienta muy poderosa para el manejo de texto en Bash. Podemos hacer lo siguiente:

```Bash
cadena="hola mundo"
echo $cadena | awk '{print toupper($0)}'
```

La salida será la misma que en el ejemplo anterior. En este caso, estamos utilizando la función `toupper` de `awk` para convertir la cadena completa en mayúsculas. También podemos aplicarla de manera más específica a cada palabra de la cadena utilizando otra función como `tolower` para convertir las letras en minúsculas.

## Profundizando en el tema

Además de los dos métodos mencionados anteriormente, también podemos capitalizar una cadena utilizando expresiones regulares en Bash. Esto nos permite una mayor flexibilidad y control sobre cómo queremos capitalizar nuestro texto.

Por ejemplo, podríamos utilizar el comando `sed` para encontrar y reemplazar patrones específicos de texto y capitalizarlos. Por ejemplo, si quisieramos convertir todas las letras después de un punto en mayúsculas, podríamos hacer lo siguiente:

```Bash
cadena="hola. este es un ejemplo."
echo $cadena | sed 's/\.\(.*\)/\.\U\1/'
```

La salida sería `Hola. Este es un ejemplo.`, ya que hemos utilizado la bandera `\U` para especificar que queremos convertir las siguientes letras en mayúsculas. Esto nos permite capitalizar palabras en medio del texto sin afectar las demás.

En resumen, la capitalización de cadenas en Bash es una tarea bastante simple y podemos utilizar diferentes métodos según nuestras necesidades y preferencias.

## Ver también

- Artículo de [Wikipedia](https://en.wikipedia.org/wiki/capitalization)
- Referencia oficial de [Bash](https://www.gnu.org/software/bash/)
- Documentación de [tr](https://linux.die.net/man/1/tr)
- Tutorial de [awk](https://www.tutorialspoint.com/awk/index.htm)