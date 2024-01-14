---
title:    "Bash: Encontrando la longitud de una cadena"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué
Algunas veces, al programar en Bash, necesitamos encontrar la longitud de una cadena de texto. Esta información puede ser útil para manipular o validar datos de entrada. En este post, te mostraremos cómo hacerlo de manera sencilla.

## Cómo hacerlo
Afortunadamente, Bash cuenta con una función integrada para encontrar la longitud de una cadena de texto. Se trata de `expr length`, seguido de la cadena entre comillas.

```
Bash
cadena="Hola mundo"
longitud=`expr length $cadena`
echo $longitud
```
El código anterior nos regresará el valor de 11, ya que la cadena "Hola mundo" cuenta con 11 caracteres.

## Profundizando
Si quieres profundizar un poco más en cómo funciona `expr length`, es importante entender que esta función cuenta espacios en blanco y caracteres especiales como parte de la longitud de la cadena. Si deseas contar únicamente los caracteres alfanuméricos, puedes combinar la función con `tr -d`, que elimina cualquier carácter que no sea alfanumérico.

```
Bash
cadena="¡Hola mundo!"
longitud=`expr length $cadena | tr -d '[:punct:]'`
echo $longitud
```

En este caso, el resultado será de 10, ya que hemos eliminado el signo de exclamación de nuestra cadena antes de contar la longitud.

## Ver también
- [Documentación de Bash: expr](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html#Bash-Conditional-Expressions)
- [Documentación de Bash: tr](https://www.gnu.org/software/bash/manual/html_node/Bash-Text-Manipulation.html#Bash-Text-Manipulation)
- [Tutorial de Bash para principiantes](https://www.tecmint.com/bash-if-statement-examples/)