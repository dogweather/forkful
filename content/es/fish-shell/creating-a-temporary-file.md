---
title:                "Creando un archivo temporal"
html_title:           "Arduino: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Crear un archivo temporal implica generar un archivo de almacenamiento temporal que se borra automáticamente después de cerrarse o al reiniciar. Facilita la manipulación de datos sin afectar el archivo original.

## Cómo hacerlo:
En Fish Shell, puedes usar el comando `mktemp`. Aquí va un ejemplo:

```Fish Shell
set archivo_temporal (mktemp)
echo "Esto es solo temporal" > $archivo_temporal
cat $archivo_temporal
```

Al ejecutar estos comandos, se imprimiría en la consola:

```Fish Shell
Esto es solo temporal
```

## Profundizando
Históricamente, la creación de archivos temporales ha sido utilizada en programación desde los tiempos de mainframes para manipular grandes volúmenes de datos. 

Alternativamente, puedes usar el comando `mktemp -d` para crear un directorio temporal, también útil para tareas temporales de lectura y escritura.

Cabe destacar que `mktemp` en Fish Shell implementa una característica segura llamada "file race condition". El archivo se crea con permisos de acceso únicos, lo que minimiza la posibilidad de conflicto entre diferentes instancias del programa que intentan acceder al archivo.

## Ver También
Para obtener más información, consulta las fuentes recomendadas:
1. [Documentación oficial de Fish Shell](http://fishshell.com/docs/current/index.html)
2. [Guía de archivos temporales en Linux](https://www.linux.com/topic/desktop/temporary-files-and-directory-fundamental-linux/)
3. [Mitigación de "File Race Condition"](https://en.wikipedia.org/wiki/Time_of_check_to_time_of_use)
4. [Principios de manipulación de archivos temporales](https://www.ibm.com/docs/es/zos/2.3.0?topic=concepts-temporary-file-handling)