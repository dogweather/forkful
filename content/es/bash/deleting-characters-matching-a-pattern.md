---
title:                "Bash: Eliminando caracteres que coinciden con un patrón"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué es importante eliminar caracteres que coinciden con un patrón?

Cuando se trabaja en Bash, a menudo se pueden encontrar cadenas de texto que contienen caracteres no deseados, como espacios o símbolos especiales. Estos caracteres pueden afectar el funcionamiento de los comandos y scripts, por lo que es necesario eliminarlos para obtener resultados precisos. En esta publicación de blog, exploraremos cómo eliminar estos caracteres que coinciden con un patrón específico en Bash.

## ¿Cómo hacerlo?

Eliminar caracteres que coinciden con un patrón puede ser fácilmente logrado utilizando el comando `sed` en Bash. Se puede usar la siguiente estructura en un script para eliminar todos los espacios de una cadena de texto dada:

```Bash
sed 's/ //g' <<< "Hola Mundo"
```

En este ejemplo, la cadena de texto "Hola Mundo" se pasa a través de la tubería al comando `sed` y se usa el patrón `s/ //g` para reemplazar todos los espacios con nada, lo que esencialmente elimina los espacios de la cadena. El resultado final será "HolaMundo". De manera similar, se pueden eliminar otros caracteres no deseados modificando el patrón según sea necesario.

Otra forma de eliminar caracteres que coinciden con un patrón es utilizando el comando `tr`, que permite especificar una lista de caracteres a reemplazar por otro. Por ejemplo, para eliminar todas las comas de una cadena de texto, se puede usar este comando:

```Bash
tr -d ',' <<< "1,2,3,4,5"
```

Esto resultará en la salida "12345" ya que todas las comas han sido eliminadas.

## Inmersión profunda: Eliminando caracteres que coinciden con un patrón

El comando `sed` utilizado en el ejemplo anterior también se puede utilizar para eliminar otros tipos de caracteres, como letras, números o símbolos específicos. Por ejemplo, si queremos eliminar todas las vocales de una cadena de texto, se puede usar el siguiente comando:

```Bash
sed 's/[aeiou]//g' <<< "Hola Mundo"
```

Esto eliminará todas las letras "a", "e", "i", "o" y "u" de la cadena de texto, lo que resultará en la salida "Hl Mnd". También se pueden utilizar rangos de caracteres y combinaciones de patrones para eliminar caracteres específicos con más precisión.

## Ver también

- Documentación de `sed`: [https://www.gnu.org/software/sed/manual/](https://www.gnu.org/software/sed/manual/)
- Documentación de `tr`: [https://www.gnu.org/software/coreutils/manual/html\_node/tr-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)