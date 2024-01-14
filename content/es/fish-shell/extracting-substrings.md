---
title:                "Fish Shell: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Al programar en Fish Shell, a menudo nos encontramos con la necesidad de extraer subcadenas de una cadena de texto más grande. Esto puede ser útil para realizar ciertas operaciones o manipulaciones en la información de manera más específica. A continuación, te mostraremos cómo puedes hacerlo de manera sencilla.

## Cómo hacerlo

Para extraer una subcadena en Fish Shell, utilizaremos el comando `string` seguido del argumento `-s` y las posiciones de inicio y fin de la subcadena que deseamos extraer. Por ejemplo, si tenemos la cadena "Hola World", y queremos extraer solamente "World", podemos utilizar el siguiente código:

```Fish Shell
set cadena "Hola World"
echo $cadena
output: Hola World
set subcadena (string -s 5 9 $cadena)
echo $subcadena
output: World
```

En este caso, especificamos que queremos extraer la subcadena empezando en la posición 5 y terminando en la posición 9. Es importante mencionar que las posiciones comienzan desde 1 y no desde 0.

También podemos utilizar el comando `substring`, que es una abreviación del comando `string`, pero la sintaxis es un poco diferente. Podemos utilizarlo de la siguiente manera:

```Fish Shell
set cadena "Hola World"
echo $cadena
output: Hola World
set subcadena $cadena[6..10]
echo $subcadena
output: World
```

En este caso, utilizamos los corchetes y los dos puntos para especificar el inicio y fin de la subcadena. Esta sintaxis puede ser más sencilla para algunos usuarios.

## Profundizando

Si queremos extraer una subcadena a partir de una palabra o letra en específico, podemos utilizar el comando `string match`. Este comando buscará la coincidencia más cercana a la palabra o letra que le indiquemos y extraerá la subcadena a partir de ahí. Por ejemplo:

```Fish Shell
set cadena "¡Hola a todos!"
echo $cadena
output: ¡Hola a todos!
set subcadena (string match a $cadena)
echo $subcadena
output: a todos!
```

Aquí, utilizamos el comando `string match` para encontrar la letra "a" en la cadena y extraer la subcadena a partir de ahí. Esto nos puede ser útil si no sabemos exactamente en qué posición se encuentra la subcadena que queremos extraer.

## Ver también

Si quieres aprender más sobre cómo manipular cadenas en Fish Shell, puedes revisar la documentación oficial y otros tutoriales en línea:

- Documentación oficial de Fish Shell: https://fishshell.com/docs/current/index.html
- Tutorial de Manipulación de Cadenas en Fish Shell (en inglés): https://scriptingosx.com/2017/05/manipulating-strings-in-fish/
- Otros tips y trucos para Fish Shell (en inglés): https://dev.to/bowmanjd/great-fish-shell-tips-i-wish-i-had-known-from-the-beginning-23o5