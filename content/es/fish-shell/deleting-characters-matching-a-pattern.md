---
title:                "Fish Shell: Eliminando caracteres que coinciden con un patrón"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por Qué?

A veces, al trabajar con archivos de texto en la terminal, puede ser necesario eliminar caracteres específicos que cumplan con un patrón determinado. Esto puede ser útil, por ejemplo, cuando se está procesando un archivo de registro y se desea eliminar todas las líneas que contengan un determinado mensaje de error.

## Cómo Hacerlo

Para eliminar caracteres que coincidan con un patrón en Fish Shell, se puede utilizar el comando `string match`. Este comando acepta una cadena de texto y un patrón de búsqueda, y elimina todas las apariciones del patrón en la cadena de texto.

Veamos un ejemplo para entender mejor. Supongamos que tenemos un archivo de registro llamado `log.txt` que contiene las siguientes líneas:

```
10:30 - Error: Connection timed out
11:00 - Info: Successfully connected
11:15 - Warning: Disk space running low
```

Si queremos eliminar todas las líneas que contengan la palabra "Error", podemos hacerlo de la siguiente manera en Fish Shell:

```
string match -v "Error" log.txt
```

Esto eliminará la primera línea del archivo y nos mostrará el resultado en la terminal:

```
11:00 - Info: Successfully connected
11:15 - Warning: Disk space running low
```

También es posible utilizar caracteres comodín en el patrón de búsqueda para eliminar múltiples caracteres que coincidan con ciertos criterios. Por ejemplo, si quisiéramos eliminar todas las líneas que comiencen con un número, podríamos utilizar el siguiente patrón:

```
string match -v "[0-9]*" log.txt
```

Esto eliminará la primera línea del archivo y nos mostrará el resultado en la terminal:

```
11:00 - Info: Successfully connected
11:15 - Warning: Disk space running low
```

## Buscando en Profundidad

Además de utilizar el comando `string match` para eliminar caracteres que coincidan con un patrón, Fish Shell también ofrece otras herramientas que pueden ser útiles en diferentes situaciones.

Por ejemplo, el comando `string replace` permite reemplazar caracteres que coincidan con un patrón en una cadena de texto. Esto puede ser útil si se desea realizar una acción más específica que simplemente eliminar los caracteres que coinciden con el patrón.

Por otro lado, para aquellos que prefieren utilizar expresiones regulares para buscar patrones, Fish Shell ofrece el comando `string match -r`, que realiza una búsqueda basada en expresiones regulares en lugar de una búsqueda de texto simple.

Explorar estas herramientas y aprender cómo utilizarlas puede ayudar a trabajar de manera más eficiente y efectiva con archivos de texto en la terminal.

## Ver También

- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial de Fish Shell en español](https://geekytheory.com/tutorial-fish-una-shell-moderna-y-rapida)
- [Guía de expresiones regulares en Fish Shell](https://fishshell.com/docs/current/tutorial.html#tutorial-rx)