---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Obtener la fecha actual en programación significa obtener la fecha y hora del momento actual del sistema. Los programadores lo hacen para registrar cuándo ocurren determinados eventos, o para realizar operaciones basadas en horarios.

## Cómo hacerlo:

```C
#include <time.h>
#include <stdio.h>

int main()
{
    // Obtén el tiempo actual
    time_t now = time(NULL);

    // Conviértelo en estructura tm
    struct tm *right_now = localtime(&now);

    // Imprime la fecha y hora
    printf("La fecha y hora actual son: %s", asctime(right_now));

    return 0;
}
```
Salida de muestra:

```C
La fecha y hora actual son: Wed Sep 15 14:06:21 2021
```

## Profundización

Históricamente las funciones de tiempo en C provienen de Unix, donde el tiempo se mide en segundos desde el primero de enero de 1970, conocido como época Unix.

Para obtener la fecha actual, C ofrece alternativas como `time()`, `localtime()` y `asctime()`. La función `time()` obtiene el tiempo actual en segundos. `Localtime()` convierte ese tiempo en una estructura `tm` más fácil de manejar. `Asctime()` toma esa estructura `tm` y la convierte en un string legible.

Una implementación detallada podría implicar el manejo de zonas horarias y ajustes para tener en cuenta el horario de verano. Sin embargo, esos son detalles avanzados más allá de esta introducción.

## Ver también:
- Documentación de C en [cplusplus.com](http://www.cplusplus.com/)
- Guía avanzada de programación en C [learn-c.org](https://www.learn-c.org/)
- [thispointer.com](https://thispointer.com/c-program-to-get-current-date-and-time-in-given-format/) para más en códigos de formato de tiempo.

---