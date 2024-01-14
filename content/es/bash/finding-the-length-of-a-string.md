---
title:    "Bash: Encontrar la longitud de una cadena"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## ¿Por qué?

Uno de los aspectos fundamentales de la programación es trabajar con cadenas de texto o strings. Sin embargo, a menudo surge la necesidad de saber cuántos caracteres componen una cadena en particular. Ya sea para realizar validaciones, manipular datos o simplemente por curiosidad, encontrar la longitud de una cadena es una habilidad importante para cualquier programador.

## Cómo hacerlo

En Bash, la forma más sencilla de encontrar la longitud de una cadena es utilizando el comando `expr length`. Veamos un ejemplo:

```Bash
cadena="¡Hola, mundo!"
echo "La longitud de la cadena es: $(expr length $cadena)"
```

La salida de este código será:

```
La longitud de la cadena es: 14
```

En este caso, el comando `expr length` toma como parámetro la variable `cadena` y nos devuelve el número de caracteres que contiene.

También es posible utilizar el operador de sustitución de comandos para almacenar el valor de la longitud en una variable, como se muestra a continuación:

```Bash
cadena="¡Hola, mundo!"
longitud=$(expr length $cadena)
echo "La longitud de la cadena es: $longitud"
```

La salida será la misma que en el ejemplo anterior.

Otra forma de encontrar la longitud de una cadena es utilizando el comando `wc`, que cuenta el número de líneas, palabras y caracteres en un archivo o una cadena. Para obtener solo el número de caracteres, utilizamos la opción `-m`, que especifica que solo queremos contar caracteres. Por ejemplo:

```Bash
cadena="¡Hola, mundo!"
echo "La longitud de la cadena es: $(echo $cadena | wc -m)"
```

La salida será de nuevo 14, ya que la cadena solo tiene una línea y una palabra.

## Profundizando

Es importante tener en cuenta que los espacios y otros caracteres especiales también se cuentan como parte de la longitud de una cadena. Además, si se utiliza una cadena multilínea, el comando `expr length` solo contará los caracteres de la primera línea.

Si deseamos encontrar la longitud de una cadena en un archivo de texto, podemos utilizar el comando `wc` con la opción `-c`. Esto nos dará la longitud en bytes del archivo.

Hay otras formas más avanzadas de encontrar la longitud de una cadena, como utilizando expresiones regulares o funciones de Bash. Sin embargo, para la mayoría de los casos, el uso de los comandos `expr length` y `wc` es suficiente.

## Ver también

- [Documentación de Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Tutorial de expressiones regulares en Bash](https://www.thegeekstuff.com/2011/07/bash-extract-data-from-file/)
- [Tutorial de funciones en Bash](https://linuxhint.com/bash_functions/)