---
title:    "Bash: Leyendo un archivo de texto"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué leer un archivo de texto en Bash?

Leer y manipular archivos de texto es una tarea común en la programación Bash. Puede ser útil para extraer datos, realizar operaciones en lote o simplemente hacer una tarea más eficiente. ¡Sigue leyendo para aprender cómo leer un archivo de texto en Bash!

## Cómo hacerlo

Para leer un archivo de texto en Bash, podemos utilizar el comando `read`. Este comando nos permite leer línea por línea un archivo y almacenar su contenido en una variable. Puedes ver un ejemplo a continuación:

```Bash
read file < archivo.txt
```

En este ejemplo, el contenido del archivo será almacenado en la variable `file` y podemos utilizarlo para realizar cualquier operación que necesitemos. Además, también podemos utilizar el comando `cat` para imprimir todo el contenido del archivo en la terminal.

## Profundizando

Ahora, es importante entender cómo Bash lee y procesa los archivos de texto. Por defecto, Bash separa cada línea de un archivo utilizando el caracter de nueva línea `\n`. Esto significa que si queremos separar el contenido de un archivo en diferentes variables, podemos utilizar `IFS` (Internal Field Separator) para cambiar el caracter de separación.

Por ejemplo, si queremos separar cada palabra de una línea en una variable diferente, podemos utilizar el siguiente comando:

```Bash
IFS=" " read -ra words < archivo.txt
```

Esto cambiará el caracter de separación a un espacio en blanco y las palabras de cada línea serán almacenadas en el array `words`.

## Ver también

- [Comandos fundamentales de Bash](https://www.hostinger.es/tutoriales/comandos-bash/)
- [Bash scripting tutorial](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Recetas útiles de Bash](https://github.com/Idnan/bash-guide)