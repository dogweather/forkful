---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

La conversión de una fecha a una cadena en programación es un proceso para cambiar una variable de tipo fecha a formato de cadena (string). Se realiza para facilitar la visualización y manipulación de dicha fecha.

## Cómo hacerlo:

A continuación, se muestra un ejemplo simple de cómo convertir una fecha a una cadena usando la función 'strftime' en C:

```C
#include <stdio.h>
#include <time.h>

int main() {
    char fecha_cadena[100];
    time_t ahora = time(NULL);
    struct tm *tiempo = localtime(&ahora);

    strftime(fecha_cadena, sizeof(fecha_cadena), "%d-%m-%Y %H:%M:%S", tiempo);

    printf("Fecha y hora actual en string: %s\n", fecha_cadena);

    return 0;
}
```

Cuando ejecutas este programa, imprimirá algo similiar a esto:

```
Fecha y hora actual en string: 29-11-2022 15:30:15
```

## Profundización:

Originalmente, en C no existía una forma nativa de convertir una fecha a cadena. Los programadores necesitaban crear sus propias funciones para este fin. Con la evolución del lenguaje, se introdujo la función 'strftime' para simplificar este proceso.

Existen alternativas a 'strftime', como 'sprintf'. Sin embargo, 'strftime' es más poderosa en la manipulación de fechas y horas debido a sus muchos formatos de conversión incorporados.

La implementación del 'strftime' implica formatear una marca de tiempo en una cadena mediante el uso de especificadores de formato, como "%d" para el día, "%m" para el mes, "%Y" para el año, "%H" para las horas, "%M" para los minutos y "%S" para los segundos. 

## Ver También:

- Para más información sobre 'strftime' y sus formatos, consulta la página del manual de C en http://man7.org/linux/man-pages/man3/strftime.3.html
- Explora más en profundidad la manipulación de fechas y tiempos en C en este tutorial: https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm