---
title:    "C++: Obteniendo la fecha actual"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo obtener la fecha actual en un programa de C++? Gran pregunta. Podrías querer obtener la fecha actual para mostrarla en una interfaz de usuario o para realizar cálculos basados en el paso del tiempo. Sea cual sea tu razón, seguir leyendo para descubrir cómo obtener la fecha actual en C++.

## Cómo hacerlo

Para obtener la fecha actual en C++, necesitarás incluir la librería `ctime`. A continuación, debes declarar una variable de tipo `time_t` y llamar a la función `time()` para asignarle el valor actual del tiempo. Después, puedes utilizar la variable `time_t` con la función `localtime()` para obtener la fecha y hora en un formato más legible.

```
#include <iostream>
#include <ctime>

int main() {

    // Declarar variable de tipo time_t
    time_t ahora;

    // Llamar a la función time() para obtener el tiempo actual
    time(&ahora);

    // Utilizar la función localtime() para obtener la fecha y hora
    struct tm *tiempo = localtime(&ahora);

    // Imprimir la fecha actual
    std::cout << "La fecha actual es: " << tiempo->tm_mday << "/" << tiempo->tm_mon + 1 << "/" << tiempo->tm_year + 1900 << std::endl;

    // Imprimir la hora actual
    std::cout << "La hora actual es: " << tiempo->tm_hour << ":" << tiempo->tm_min << ":" << tiempo->tm_sec << std::endl;

    return 0;
}
```

La salida de este programa sería algo así:

```
La fecha actual es: 29/9/2021
La hora actual es: 11:30:45
```

## Profundizando

Ahora, profundicemos un poco más en el código que acabamos de ver. La función `time()` que utilizamos para obtener el tiempo actual devuelve un valor entero de tipo `time_t` que representa la cantidad de segundos transcurridos desde la época UNIX (1 de enero de 1970). Por lo tanto, al llamar a esta función y asignar su valor a una variable `time_t`, estamos obteniendo el tiempo actual en segundos.

Luego, utilizamos la función `localtime()` para convertir este valor de segundos en una estructura `tm` que contiene información sobre la fecha y hora actual en un formato más legible. Por ejemplo, `tiempo->tm_mday` representa el día del mes, `tiempo->tm_mon` representa el mes (teniendo en cuenta que enero es el mes 0) y `tiempo->tm_year` representa el año (teniendo en cuenta que el año 1900 es el año 0).

Por último, es importante mencionar que la salida de la función `localtime()` depende del idioma y la configuración regional de tu sistema operativo. Por lo tanto, la salida en tu máquina puede variar.

## Ver también

- [Documentación de la librería `ctime`](https://www.cplusplus.com/reference/ctime/)
- [Tutorial de C++ en SoloLearn](https://www.sololearn.com/learning/1051)
- [Video tutorial de Introducción a C++ de freeCodeCamp](https://www.youtube.com/watch?v=vLnPwxZdW4Y)