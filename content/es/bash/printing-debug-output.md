---
title:                "Imprimiendo salida de depuración"
html_title:           "Arduino: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué es y Por Qué?

Imprimir información de depuración en Bash es simplemente mostrar mensajes en la terminal sobre lo que sucede durante la ejecución de nuestro script. Los programadores lo hacen para identificar rápidamente los errores y conocer exactamente dónde y por qué ocurrieron.

## Cómo hacerlo:

Los mensajes de depuración se pueden imprimir utilizando el comando `echo` o `printf`. Aquí te dejo un ejemplo simple:

```Bash
#!/bin/bash
debug=1

if [[ $debug -eq 1 ]]
then
    echo "Modo de depuración activado"
fi
```

Si ejecutas este script, verás el siguiente output:

```Bash
Modo de depuración activado
```

## En Profundidad

Aunque la depuración en Bash puede parecer simple, hay un contexto histórico importante que recordar. Bash se originó en los sistemas operativos Unix a finales de la década de 1980, y el `echo` era una de las pocas formas de emitir información a la pantalla, de ahí que sea una técnica de depuración común.

En lugar del `echo`, también puedes usar `printf`, que ofrece un control más granular sobre el formato del mensaje. Además, puedes utilizar la opción `-xv` al ejecutar el script para ver cómo se ejecuta línea por línea, lo que es especialmente útil para el depurado.

El debug también puede ser implementado con el uso de `trap`, una función incorporada de Bash que permite definir acciones personalizadas para señales específicas.

## Ver También

1. TutorialsPoint – Debugging Bash Scripts: (https://www.tutorialspoint.com/unix/unix-debugging.htm).

2. GNU – Debugging Bash Scripts: (https://www.gnu.org/software/bash/manual/html_node/Traps.html).