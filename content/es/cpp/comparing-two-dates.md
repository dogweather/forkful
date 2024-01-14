---
title:    "C++: Comparando dos fechas"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por qué comparar dos fechas en C++

Comparar dos fechas es una tarea común en la programación, especialmente cuando se trabaja con fechas de eventos o plazos. Esta tarea puede ayudar a identificar si una fecha es anterior, posterior o igual a otra, lo que permite tomar decisiones basadas en el orden cronológico de los eventos.

## Cómo hacerlo

Para comparar dos fechas en C++, se pueden seguir los siguientes pasos:

1. Crear dos objetos de la clase `tm`, que representan las fechas que se desean comparar.
2. Utilizar la función `mktime()` para convertir cada objeto `tm` en un objeto de tipo `time_t`.
3. Comparar los dos objetos `time_t` utilizando los operadores de comparación (`<`, `>`, `==`). Esto devolverá un valor booleano que indicará si una fecha es anterior, posterior o igual a la otra.

A continuación, se muestra un ejemplo de código que compara dos fechas y muestra el resultado en la consola:

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main()
{
    // Crear dos objetos tm
    struct tm fecha1 = {0, 0, 0, 1, 1, 2020 - 1900};
    struct tm fecha2 = {0, 0, 0, 1, 1, 2021 - 1900};
    
    // Convertir a objetos time_t
    time_t time1 = mktime(&fecha1);
    time_t time2 = mktime(&fecha2);

    if (time1 < time2) {
        cout << "La fecha 1 es anterior a la fecha 2" << endl;
    } else if (time1 > time2) {
        cout << "La fecha 1 es posterior a la fecha 2" << endl;
    } else {
        cout << "Las fechas son iguales" << endl;
    }

    return 0;
}
```

El resultado de este código sería:

```
La fecha 1 es anterior a la fecha 2
```

## Profundizando

Además de comparar fechas con los operadores de comparación, también se pueden utilizar las funciones `difftime()` y `gmtime()` para obtener la diferencia en segundos entre dos fechas y obtener una representación estructurada del tiempo, respectivamente.

Por ejemplo, si queremos obtener la diferencia en días entre dos fechas, podemos utilizar la función `difftime()` y luego dividir el resultado entre 86400 (que es la cantidad de segundos en un día).

```C++
// Obtener la diferencia en días
double diff = difftime(time2, time1);
int diff_dias = diff / 86400;
``` 

También se pueden utilizar diferentes formatos de tiempo con `strftime()` para obtener una representación legible de las fechas.

Para más información sobre cómo trabajar con fechas en C++, puedes consultar la documentación oficial de la biblioteca de tiempo de C++.

## Ver también

- [Biblioteca de tiempo de C++ en cplusplus.com](https://www.cplusplus.com/reference/ctime/?kw=time)
- [Manipulación de fechas en C++ en tutorialspoint.com](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)
- [Ejemplos de comparación de fechas en C++ en geeksforgeeks.org](https://www.geeksforgeeks.org/comparing-two-dates-date-comparision-in-c-c/)