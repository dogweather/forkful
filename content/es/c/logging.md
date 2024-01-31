---
title:                "Registro de Actividades en Programación"
date:                  2024-01-26T01:00:05.923198-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registro de Actividades en Programación"

category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/logging.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
El registro (logging) es básicamente anotar lo que está haciendo tu programa, típicamente escribiendo mensajes a un archivo o terminal. Los programadores lo hacen para hacer seguimiento de eventos, diagnosticar problemas y para tener un registro de auditoría que cuenta la historia de la operación de una aplicación a lo largo del tiempo.

## Cómo hacerlo:
Comencemos con algunos conceptos básicos. C no tiene un marco de trabajo de registro incorporado, pero puedes armar algo simple con `stdio.h`. Aquí te muestro cómo:

```c
#include <stdio.h>
#include <time.h>

void logMessage(const char* message) {
    time_t now;
    time(&now);
    char *date = ctime(&now);
    date[strlen(date) - 1] = '\0'; // Elimina el salto de línea al final del resultado de ctime()
    printf("[%s] %s\n", date, message);
}

int main() {
    logMessage("La aplicación ha iniciado.");
    // ... tu código va aquí ...
    logMessage("La aplicación está haciendo algo importante.");
    // ... tu código continúa ...
    logMessage("La aplicación ha terminado.");
    return 0;
}
```

Una salida de muestra podría verse así:

```
[Tue Mar 9 12:00:01 2023] La aplicación ha iniciado.
[Tue Mar 9 12:00:02 2023] La aplicación está haciendo algo importante.
[Tue Mar 9 12:00:03 2023] La aplicación ha terminado.
```

Por supuesto, en el mundo real probablemente querrías escribir en un archivo en lugar de la terminal, manejar diferentes niveles de registro, y tal vez usar una biblioteca predefinida.

## Estudio Profundo
El registro en C tiene un encanto particular: es tan de bajo nivel como la mayor parte del resto del lenguaje. Históricamente, el registro se realizaba utilizando `fprintf` con `stderr` o un puntero a un archivo. A medida que los programas se volvían más complejos, también lo hacían las necesidades de registro, lo que llevó al desarrollo de bibliotecas como `syslog` en sistemas Unix, que podrían manejar registros de múltiples fuentes con varios niveles de importancia.

En el panorama moderno, existen muchas bibliotecas de registro en C, como `zlog`, `log4c` y `glog`, que ofrecen un conjunto de características rico que incluye rotación de registros, registro estructurado y registro multihilo. Estas soluciones permiten un control detallado sobre la verbosidad, destinos y formatos del registro.

Al implementar un sistema de registro, se deben considerar detalles como el formato de las marcas de tiempo, la gestión de archivos de registro y el rendimiento. Registrar las marcas de tiempo en los registros es crucial para correlacionar eventos, mientras que la rotación de registros asegura que los archivos de registro no consuman demasiado espacio en disco. El acto de registro también debe ser rápido y no bloquear el flujo principal de la aplicación para evitar que el registro se convierta en un cuello de botella.

## Ver También
Para profundizar más en bibliotecas y prácticas de registro en C, consulta estos recursos:

- Manual de `syslog` de GNU: https://www.gnu.org/software/libc/manual/html_node/Syslog.html
- `zlog`: Una biblioteca de registro altamente configurable para C - https://github.com/HardySimpson/zlog
- `log4c`: Un marco de trabajo de registro para C modelado después de Log4j - http://log4c.sourceforge.net/
- `glog`: La biblioteca de registro a nivel de aplicación de Google - https://github.com/google/glog
