---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "C: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Calcular una fecha en el futuro o pasado implica realizar operaciones, generalmente aritméticas, con fechas. Los programadores lo hacen para manejar intervalos de tiempo, como periodos de suscripción o programar eventos futuros.

## Como se Hace:

Aquí está un pequeño ejemplo para calcular una fecha en el futuro usando la biblioteca `time.h` en C:

```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm fecha_futura;
    time_t ahora;

    time(&ahora);
    fecha_futura = *localtime(&ahora);

    // Añadiendo 10 días
    fecha_futura.tm_mday += 10;

    mktime(&fecha_futura);
    printf("La fecha en 10 días será: %s", asctime(&fecha_futura));

    return 0;
}
```

Este programa te dará un resultado como este:
```
La fecha en 10 días será: Thu Feb 18 15:27:33 2022
```

## Buceo profundo

Historicamente, la manipulación de fechas en C era bastante complicada debido a la falta de una biblioteca dedicada. Esto cambió con la introducción de `time.h`, aunque aún requiere un cierto nivel de entendimiento para ser usada apropiadamente.

Existen alternativas, puedes recurrir a bibliotecas de terceros más amigables, como `libdate` o `Boost.Date_Time` si estás trabajando con C++. Sin embargo, `time.h` es suficiente para la mayoría de los casos.

Aun así, hay que tener en cuenta los detalles de implementación. Por ejemplo, la función `mktime` ajusta los valores de las estructuras `tm` cuando se sale del rango permitido. Esto es útil cuando sumas días, semanas, meses o años a una fecha.

## Ver También

Para obtener más información sobre las fechas en C, consulte los siguientes recursos:

- Documentación de `time.h`: www.cplusplus.com/reference/ctime/
- Guía de referencia de C: www.cprogramming.com/tutorial/c/lesson14.html
- Tutorial de Bibliotecas de Tiempo en C: www.learn-cocoa.org/dl/cnprog/AppendixE.pdf
- Guía de Programadores de Unix - Fecha y Hora: www.unix.com/man-page/all/1/date/