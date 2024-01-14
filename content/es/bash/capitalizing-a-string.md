---
title:                "Bash: Capitalizando una cadena"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena en Bash?

Capitalizar una cadena en Bash puede ser útil en muchos casos, especialmente si estás trabajando con texto y quieres que tu código sea más legible. Por ejemplo, si tienes una lista de nombres en minúsculas y quieres que todas las primeras letras sean mayúsculas, capitalizar la cadena te facilitará mucho el trabajo.

## Cómo capitalizar una cadena en Bash

Para capitalizar una cadena en Bash, puedes utilizar el comando `tr` (traductor), que te permite realizar traducciones y transformaciones de caracteres. El siguiente ejemplo muestra cómo capitalizar la primera letra de una cadena:

```Bash
cadena="hola mundo"
cadena_capitalizada=$( echo $cadena | tr '[:lower:]' '[:upper:]' )
echo $cadena_capitalizada
```

El resultado de este comando será `Hola mundo`.

También puedes utilizar el comando `sed` (editor de texto) para capitalizar una cadena en Bash. Aquí hay un ejemplo de cómo hacerlo:

```Bash
cadena="hola mundo"
cadena_capitalizada=$( echo $cadena | sed 's/\b\w/\u&/g' )
echo $cadena_capitalizada
```

Este comando también producirá `Hola mundo` como resultado.

## Profundizando en la capitalización de cadenas

Hay varias formas de capitalizar una cadena en Bash, y cada una tiene sus ventajas y desventajas. El uso de `tr` y `sed` es solo una forma, pero también puedes utilizar otras herramientas, como el comando `awk` (analizador de textos). Además, si quieres capitalizar una cadena con más de una palabra, deberás utilizar un bucle o una función para recorrer y capitalizar cada palabra individualmente.

Otra cosa a tener en cuenta al capitalizar una cadena es el idioma. Algunos idiomas, como el español, tienen reglas específicas para la capitalización, por lo que es importante tener esto en cuenta al escribir tu código. Puedes consultar la documentación de Bash para obtener más información sobre cómo capitalizar cadenas en diferentes idiomas.

## Ver también
- [Documentación oficial de Bash](https://www.gnu.org/software/bash/manual/html_node/index.html)
- [Tutorial de Bash para principiantes](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Guía para manejar cadenas en Bash](https://www.baeldung.com/linux/bash-string-manipulation)