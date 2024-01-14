---
title:    "C: Comparando dos fechas"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por qué

Comparar dos fechas es una tarea común en la programación, especialmente cuando se trabaja con aplicaciones que manejan eventos o citas. Al comparar dos fechas, podemos determinar si un evento ha pasado, está sucediendo en el momento presente o si sucederá en el futuro. En este artículo, aprenderás cómo comparar dos fechas en el lenguaje de programación C y cómo profundizar en el concepto de comparación de fechas.

## Cómo

En C, podemos comparar dos fechas utilizando la función `difftime()` de la biblioteca `time.h`. Esta función toma dos parámetros, las dos fechas que queremos comparar, y devuelve la diferencia en segundos entre ellas. Veamos un ejemplo para entenderlo mejor:

```
#include <stdio.h>
#include <time.h>

int main() {

    // Definir dos estructuras tm para almacenar las fechas
    struct tm fecha1 = {0};
    struct tm fecha2 = {0};

    // Asignar valores a las estructuras de fecha
    // Consideremos que fecha1 es el 10 de enero de 2020 y fecha2 es el 20 de enero de 2020
    fecha1.tm_year = 2020;
    fecha1.tm_mday = 10;
    fecha1.tm_mon = 0; // enero es el mes 0
    fecha2.tm_year = 2020;
    fecha2.tm_mday = 20;
    fecha2.tm_mon = 0;

    // Calcular la diferencia en segundos entre las dos fechas
    double diferencia = difftime(mktime(&fecha2), mktime(&fecha1));

    // Imprimir la diferencia
    printf("La diferencia entre las dos fechas es de %.0f segundos\n", diferencia);

    // Comparar las fechas
    if(diferencia > 0) {
        printf("La fecha 2 es posterior a la fecha 1\n");
    }
    else if(diferencia < 0) {
        printf("La fecha 1 es posterior a la fecha 2\n");
    }
    else {
        printf("Las dos fechas son iguales\n");
    }

    return 0;
}
```

**Salida:**

```
La diferencia entre las dos fechas es de 864000 segundos
La fecha 2 es posterior a la fecha 1
```

En el ejemplo anterior, primero definimos dos estructuras `tm` para almacenar nuestras fechas. Luego asignamos valores a cada una de ellas y utilizamos la función `difftime()` para calcular la diferencia en segundos. Finalmente, comparamos las fechas utilizando la diferencia obtenida.

Otra forma de comparar fechas en C es convertirlas a su forma de representación numérica y luego realizar la comparación. Por ejemplo, en lugar de utilizar la función `mktime()`, podemos utilizar directamente el número de días transcurridos desde una fecha de referencia determinada (como el 1 de enero de 1970).

## Profundizando

Al comparar fechas, es importante tener en cuenta el formato y la precisión en la que se están almacenando las fechas. Por ejemplo, si estás trabajando con una fecha que incluye la hora, es necesario tener en cuenta también los minutos y segundos al realizar la comparación para obtener resultados precisos.

Además, debemos tener cuidado con el manejo de los años bisiestos, ya que el mes de febrero tiene un número variable de días.

Por último, es importante recordar que las fechas son un concepto abstracto y su representación numérica puede variar en diferentes culturas y zonas horarias. Por lo tanto, es importante tener en cuenta el uso de funciones como `gmtime()` o `localtime()` para obtener la fecha y hora en un formato adecuado según las preferencias del usuario.

## Ver también

- [Documentación de la función `difftime()` en C](https://www.tutorialspoint.com/c_standard_library/c_function_difftime.htm)
- [Especificación completa de la biblioteca `time.h` en la referencia del lenguaje C](https://www.gnu.org/software/libc/manual/html_node/Time.html#Time)
- [Comparación de fechas en otros lenguajes de programación](https://medium.com/@codeloggy/comparing-dates-in-different-programming-languages-24af6214e5a5)