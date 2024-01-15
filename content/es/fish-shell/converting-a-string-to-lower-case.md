---
title:                "Convertir una cadena a minúsculas"
html_title:           "Fish Shell: Convertir una cadena a minúsculas"
simple_title:         "Convertir una cadena a minúsculas"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Has recibido un texto todo en mayúsculas y quieres convertirlo a minúsculas. O tal vez estás creando un programa que necesita que los datos estén en un formato específico. En cualquier caso, convertir una cadena de texto a minúsculas es una tarea común y útil en la programación.

## Cómo hacerlo

La Fish Shell, al igual que otros intérpretes de línea de comandos, ofrece diferentes métodos para manipular cadenas de texto. Para convertir una cadena a minúsculas en Fish Shell, puedes usar el comando `string tolower`.

```
Fish Shell string tolower
```

Este comando toma la cadena de texto a la que deseas aplicar la transformación y la imprime en minúsculas. Por ejemplo, si tienes el texto "ESPAÑA" y lo ejecutas con `string tolower`, obtendrás como resultado "españa".

Otra forma de lograr lo mismo es utilizando el comando `awk` y la función `tolower()`. El siguiente código muestra cómo se vería esto:

```
Fish Shell awk '{print tolower($0)}'
```

Nuevamente, al ejecutarlo con el texto "ESPAÑA", obtendrías "españa" como resultado.

## Profundizando

Si deseas convertir una cadena de texto a minúsculas en un script de Fish Shell, puedes hacerlo utilizando variables y la sintaxis de sustitución de comandos. Por ejemplo, supongamos que tienes una variable `nombre` que contiene "JUAN" y quieres convertirlo a "juan". Podrías hacerlo de la siguiente manera:

```
set nombre "JUAN"
set nombre (string tolower $nombre)
echo $nombre
```

El resultado de esto sería "juan". Aquí, utilizamos `set` para asignar un valor a la variable `nombre` y luego utilizamos `string tolower` como un comando dentro de la variable para aplicar la transformación.

## Ver también

- [Documentación de Fish Shell](https://fishshell.com/docs/current/)
- [Cómo manipular cadenas de texto en Fish Shell](https://www.freecodecamp.org/news/how-to-manipulate-strings-in-the-fish-shell/)