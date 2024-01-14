---
title:    "Bash: Obteniendo la fecha actual"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué obtener la fecha actual en programación Bash

Si estás aprendiendo a programar en Bash, es importante que sepas cómo obtener la fecha actual en tus scripts. Esta información puede ser útil para varias tareas, como la organización de archivos por fecha, la creación de logs o simplemente para fines de registro en tus programas.

## Cómo hacerlo

Para obtener la fecha actual en Bash, puedes usar el comando `date` seguido de una formato específico para la fecha que desees. Por ejemplo, si quieres obtener una salida con el formato "día-mes-año", puedes escribir lo siguiente en tu script:

```Bash
fecha_actual=$(date +"%d-%m-%Y")
echo "La fecha de hoy es $fecha_actual"
```

La variable `fecha_actual` almacenará el resultado del comando `date` y luego se mostrará en la consola el mensaje "La fecha de hoy es XX-XX-XXXX", donde XX son los números correspondientes al día, mes y año actual.

Otra opción es utilizar el comando `date` sin ningún formato específico, lo cual mostrará la fecha en un formato predeterminado, generalmente mes-día-año, pero esto puede variar dependiendo de la configuración de tu sistema.

```Bash
fecha_actual=$(date)
echo "La fecha de hoy es $fecha_actual"
```

## Profundizando

Si quieres aprender más sobre cómo obtener la fecha actual en Bash, puedes explorar las opciones disponibles en el comando `date`. Por ejemplo, puedes incluir la hora o la zona horaria en la salida, así como especificar un formato personalizado utilizando caracteres especiales.

Además, también puedes utilizar el comando `cal` para mostrar un calendario en la consola, lo cual puede ser útil si necesitas trabajar con fechas específicas.

En resumen, obtener la fecha actual en Bash es una tarea sencilla pero útil en tus scripts. Continúa explorando las opciones disponibles para personalizar la salida y aprovecha esta herramienta en tus proyectos.

## Ver también

- Tutorial sobre los comandos `date` y `cal` en Bash: [https://likegeeks.com/es/tipos-de-formato-de-fecha-en-bash/](https://likegeeks.com/es/tipos-de-formato-de-fecha-en-bash/)
- Documentación oficial de Bash sobre el comando `date`: [https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- Artículo sobre cómo utilizar el comando `cal` en Linux: [https://www.muylinux.com/2019/10/05/el-comando-cal-de-linux-y-sus-opciones/](https://www.muylinux.com/2019/10/05/el-comando-cal-de-linux-y-sus-opciones/)