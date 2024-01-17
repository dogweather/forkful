---
title:                "Eliminando caracteres que coinciden con un patrón."
html_title:           "Fish Shell: Eliminando caracteres que coinciden con un patrón."
simple_title:         "Eliminando caracteres que coinciden con un patrón."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Eliminar caracteres que coincidan con un patrón en un código es un proceso útil para los programadores, ya que les permite manipular y limpiar datos de manera eficiente y precisa.

## ¿Cómo hacerlo?

Aquí te mostramos dos formas de eliminar caracteres que coincidan con un patrón en Fish Shell:

Fish Shell ofrece un comando integrado para eliminar caracteres que coincidan con un patrón específico. Simplemente usa el siguiente formato:

```
Fish Shell
string replace "patrón" "nuevo carácter" -- "cadena original"
```

Por ejemplo, si tienes una cadena de texto que contiene números separados por comas y quieres reemplazar las comas por espacios, puedes usar el siguiente comando:

```
Fish Shell
string replace "," " " -- "1,2,3"
```

Esto te dará una salida de: `1 2 3`.

Otra opción es utilizar el comando `sed`, que también es compatible con Fish Shell. En este caso, el formato sería el siguiente:

```
Fish Shell
sed "s/ patrón /nuevo carácter/g" "cadena original"
```

Siguiendo con el mismo ejemplo anterior, puedes usar `sed` de la siguiente manera para lograr el mismo resultado:

```
Fish Shell
sed "s/,/ /g" "1,2,3"
```

## Profundizando

Este proceso de eliminar caracteres que coincidan con un patrón es una técnica común en la programación y ha sido utilizado por décadas en diferentes lenguajes de programación como UNIX y Perl.

Además de los métodos mencionados anteriormente, también puedes utilizar otras herramientas de manipulación de texto en Fish Shell, como `grep` y `awk`, para lograr resultados similares.

Para los desarrolladores interesados en los detalles técnicos, Fish Shell utiliza expresiones regulares y el comando `tr` detrás de escena para lograr esta funcionalidad.

## Ver también

- [Documentación de string replace en Fish Shell](https://fishshell.com/docs/current/commands.html#string-replace)
- [Guía de expresiones regulares en Fish Shell](https://fishshell.com/docs/current/index.html#regexp)
- [Manipulación de texto en Fish Shell](https://fishshell.com/docs/current/tutorial.html#execute-commands-in-the)