---
title:                "Bash: Escribiendo en error estándar"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por Qué

Escribir en el error estándar en Bash es una habilidad importante para cualquier programador. Al hacer uso de esta técnica, podemos mostrar mensajes de error y depuración en nuestros programas para facilitar el proceso de identificación y solución de problemas.

## Cómo Hacerlo

Para escribir en el error estándar en Bash, utilizamos el comando `>&2` seguido de un mensaje entre comillas. Por ejemplo:

```Bash
echo "Este es un mensaje de error" >&2
```

Este comando enviará el mensaje especificado al error estándar en lugar de a la salida estándar. Si ejecutamos este comando en nuestro terminal, no veremos nada impreso en la pantalla, ya que el error estándar no se muestra por defecto. Sin embargo, si combinamos este comando con la redirección del error estándar a un archivo, podemos crear un archivo de registro de errores.

```Bash
ls archivos_noexistentes 2> registro_errores.txt
```

Este comando ejecutará el comando `ls` con una entrada inválida y redireccionará cualquier mensaje de error al archivo `registro_errores.txt`.

## Profundizando

En ocasiones, podemos encontrarnos con la necesidad de mostrar mensajes de error personalizados en nuestros scripts de Bash. Para hacer esto, podemos utilizar el comando `exit` seguido de un número de salida. Por convención, se utiliza el número `1` para indicar errores generales y números mayores para errores específicos. Por ejemplo:

```Bash
if [ $# -eq 0 ]; then
  echo "Debe proporcionar un parámetro" >&2
  exit 1
fi
```

Este script comprueba si se proporciona un parámetro en la línea de comandos y si no, muestra un mensaje de error personalizado y finaliza la ejecución con un estado de salida `1`.

## Ver También

- [Documentación Oficial de Bash sobre Redirecciones] (https://www.gnu.org/software/bash/manual/html_node/Redirections.html)
- [Tutorial de Programación de Bash para Principiantes] (https://ryanstutorials.net/bash-scripting-tutorial/bash-script.php)
- [Guía de Depuración de Bash] (https://www.linuxjournal.com/content/debugging-bash-scripts)