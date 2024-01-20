---
title:                "Comprobando si existe un directorio"
html_title:           "Bash: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Verificar si un directorio existe es comprobar si hay una carpeta con este nombre en el sistema. Los programadores lo hacen para evitar errores al intentar acceder o manipular directorios que no están.

## Cómo hacerlo:
Aquí te muestro cómo verificar si un directorio existe:

```Bash
if [ -d "/ruta/al/directorio" ]; then
  echo "El directorio existe."
else
  echo "El directorio no existe."
fi
```

Si el directorio existe, verás:
```
El directorio existe.
```

Si no existe, obtendrás:
```
El directorio no existe.
```

## Profundizando
Históricamente, el comando `test` (también representado por `[ ]`) ha sido utilizado en Unix y sus descendientes para evaluar expresiones condicionales.  `-d` es uno de los muchos operadores que test puede usar; específicamente, verifica si existe un directorio.

Alternativas a `[ -d "/ruta/al/directorio" ]` incluyen usar `[[ ]]` para una versión más moderna de prueba incorporada en Bash, o `find` para scripts más complejos. Podrías también utilizar scripts en Perl o Python para tareas más avanzadas, pero para una simple verificación Bash es suficiente y rápido.

En la implementación, es importante considerar si solamente quieres saber si un directorio existe, o si también necesitas verificar si tienes permisos para leer o escribir en él.

## Véase también
- Documentación de Bash para pruebas condicionales: https://www.gnu.org/software/bash/manual/bash.html#Conditional-Constructs
- Tutorial avanzado de scripting en Bash: http://tldp.org/LDP/abs/html/
- Información sobre el comando `find`: https://man7.org/linux/man-pages/man1/find.1.html