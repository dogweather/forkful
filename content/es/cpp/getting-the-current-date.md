---
title:                "Obteniendo la fecha actual"
html_title:           "C++: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué se usa?

Obtener la fecha actual es una función importante en la programación, ya que permite a los programadores obtener la fecha y hora actual en su código. Esto puede ser útil en muchas situaciones, como en el registro de eventos o para mostrar la fecha en una interfaz de usuario.

## Cómo hacerlo:

En C++, para obtener la fecha actual se utiliza la función `time()`, que se encuentra en la biblioteca `ctime`. Aquí un ejemplo de cómo se puede utilizar esta función:

```C++
#include <ctime>

int main() {
  // Obtener el tiempo actual en segundos desde 1970
  time_t now = time(0);
  
  // Convertir el tiempo a una cadena de caracteres
  char* date = ctime(&now);
  
  // Imprimir la fecha actual
  std::cout << "La fecha y hora actuales son: " << date << std::endl;
  
  return 0;
}
```

Este código imprimirá la fecha y hora actuales en el formato de día de la semana, mes, día, hora y año. Por ejemplo: `Mié Oct 13 15:25:00 2021`.

## Detalles a fondo:

La función `time()` se basa en el llamado *Epoch time*, que es el número de segundos transcurridos desde el 1 de enero de 1970 a las 00:00:00 UTC. Esto se debe a que es una fecha fácilmente convertible y compatible con diferentes sistemas operativos.

Además de `ctime`, también existen otras funciones para obtener la fecha actual, como `localtime()` y `gmtime()`, que permiten obtener la fecha en formatos específicos.

Alternativamente, existen librerías como *boost* y *chrono* que también proporcionan funciones para trabajar con fechas y tiempos en C++.

## Vea también:

- [Ejemplo detallado de obtener la fecha actual en C++](https://www.programiz.com/cpp-programming/library-function/ctime/time)
- [Información sobre el Epoch time](https://en.wikipedia.org/wiki/Unix_time)
- [Documentación de la biblioteca *boost* para trabajar con fechas y tiempos](https://www.boost.org/doc/libs/1_77_0/doc/html/date_time.html)