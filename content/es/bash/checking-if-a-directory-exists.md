---
title:                "Verificando si un directorio existe"
html_title:           "Bash: Verificando si un directorio existe"
simple_title:         "Verificando si un directorio existe"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?

La verificación de la existencia de un directorio es una tarea común en la programación Bash. Los programadores verifican si un directorio existe para evitar errores de sintaxis al intentar acceder o manipular directorios inexistentes.

## ¿Cómo se hace?

Los códigos siguientes muestran cómo verificar la existencia de un directorio en Bash.

```Bash
# verificar si un directorio existe
if [ -d "/ruta/directorio" ]; then
    echo "El directorio existe."
else
    echo "El directorio no existe."
fi
```

En este ejemplo, si el "directorio" en "/ruta/directorio" existe, se imprime "El directorio existe." De lo contrario, se imprime "El directorio no existe."

## Profundización

Historicamente, Bash incluye la capacidad de verificar la existencia de un directorio desde sus primeras versiones. Esta funcionalidad es vital en muchas tareas de scripting, como navegación de archivos, respaldo de datos, y manipulación de archivos y directorios. 

Existen alternativas a la verificación de la existencia de un directorio en Bash. Por ejemplo, se puede usar un enfoque orientado a objetos en lenguajes de programación más modernos, como Python o Ruby, pero estas suelen ser sobre-dimensionadas para tareas simples.

En cuanto a los detalles de implementación, el `-d` en el script es una opción de test que comprueba si el directorio existe. Bash interpreta diferentes opciones de prueba para verificar diversos tipos de ficheros, por lo que otra opción diferente a `-d` daría un resultado distinto.

## Ver también

Puedes continuar tu aprendizaje en Bash y cómo manejar directorios con los siguientes recursos:

- Guía de Bash para principiantes: https://www.tldp.org/LDP/Bash-Beginners-Guide/html/index.html
- Comandos de Bash para manipulación de directorios: https://www.lifewire.com/linux-commands-for-navigating-file-system-4027320
- Bash scripting: https://www.shellscript.sh