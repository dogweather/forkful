---
title:    "C: Obteniendo la fecha actual"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Por qué obtener la fecha actual es importante en la programación?

Al escribir programas, a menudo se requiere conocer la fecha actual. Ya sea para realizar cálculos, registrar eventos o simplemente para fines de organización, es vital poder obtener la fecha actual en un programa. Afortunadamente, en lenguaje C, existen funciones específicas que nos permiten obtener fácilmente la fecha actual. En esta entrada, aprenderemos cómo realizar esta tarea de manera eficaz y algunos detalles más profundos sobre cómo funciona.

## ¿Cómo obtener la fecha en C?

Para obtener la fecha en C, utilizaremos la función `time()` que se encuentra en la biblioteca `time.h`. Esta función devuelve un valor de tipo `time_t` que representa el número de segundos transcurridos desde el 1 de enero de 1970 a las 00:00:00 UTC. A continuación, se muestra un ejemplo de código que utiliza la función `time()` para obtener la fecha actual y luego la muestra por pantalla en un formato legible:

```C
#include <stdio.h>
#include <time.h>

int main() {
  // Obtenemos la fecha actual utilizando la función time()
  time_t fecha_actual = time(NULL);

  // Utilizamos la función ctime() para convertir la fecha en un formato legible
  printf("La fecha actual es: %s", ctime(&fecha_actual));

  return 0;
}
```

La función `ctime()` convierte la fecha actual en una cadena de caracteres con un formato como el siguiente: `Thu Jul 29 21:27:47 2021`. Si bien está en inglés, podemos utilizar otras funciones como `strftime()` para mostrar la fecha en un idioma específico. Estas son solo algunas de las posibilidades que nos brinda la biblioteca `time.h`.

## Profundizando en la obtención de la fecha en C

Ahora que ya conocemos cómo obtener la fecha actual en C, es importante tener en cuenta algunos detalles sobre cómo funciona internamente. Como mencionamos anteriormente, la función `time()` devuelve un valor de tipo `time_t`, que se almacena en una variable.

Este valor representa el número de segundos transcurridos desde una fecha de referencia hasta la fecha actual. La fecha de referencia siempre es el 1 de enero de 1970 a las 00:00:00 UTC, también conocida como la "época de Unix". Debido a que los valores de tipo `time_t` son números enteros, pueden ser representados por un número binario de 32 bits (en sistemas de 32 bits) o de 64 bits (en sistemas de 64 bits). 

La función `time()` llama a un reloj interno del sistema que continúa contando los segundos desde la época de Unix y devuelve ese valor. Por lo tanto, cada vez que llamamos a `time()`, obtenemos un valor diferente que representa la fecha y hora exacta en la que se realizó la llamada.

## Vea también

- [Documentación de la función `time()` en C](https://www.tutorialspoint.com/c_standard_library/c_function_time.htm)
- [Más información sobre la "época de Unix"](https://www.epochconverter.com/epoch/1970-01-01)
- [Tutorial de programación en C en español](https://es.coursera.org/learn/programando-en-c)