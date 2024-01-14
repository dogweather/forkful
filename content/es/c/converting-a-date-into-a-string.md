---
title:                "C: Convirtiendo una fecha en una cadena."
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una fecha en una cadena de texto es un paso clave en el desarrollo de programas en lenguaje C. Esta transformación permite mostrar la información de la fecha de una manera legible para los usuarios, así como también facilita la manipulación y cálculo de fechas en el código.

## Cómo hacerlo

Para realizar esta conversión en C, podemos utilizar la función `strftime()` de la biblioteca estándar `time.h`. Esta función toma tres argumentos: un puntero a una cadena de caracteres donde se almacenará la fecha en formato de texto, el tamaño máximo de la cadena y un formato especificando cómo se desea que se muestre la fecha.

Un ejemplo de código para convertir la fecha actual en una cadena de texto se vería así:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t now = time(NULL);
    char date[50];
    strftime(date, 50, "%A, %d de %B de %Y", localtime(&now));
    printf(date);
    return 0;
}
```

En este ejemplo, se está utilizando el formato "%A, %d de %B de %Y" para mostrar la fecha en formato día de la semana, día del mes, mes y año. Al ejecutar el programa, se obtendría la siguiente salida:

```
Lunes, 15 de octubre de 2018
```

## Profundizando

La función `strftime()` ofrece una amplia variedad de opciones para personalizar el formato de la fecha. Por ejemplo, se puede cambiar el orden en que se muestran los elementos de la fecha, agregar información como la hora o el año con dos dígitos, entre otras posibilidades.

Además, es importante tener en cuenta que la función `strftime()` solo funciona con fechas locales, por lo que si se desea convertir una fecha en horario UTC, se debe utilizar la función `gmtime()` en lugar de `localtime()`.

Para obtener más información y ejemplos de cómo utilizar la función `strftime()`, se recomienda consultar la documentación oficial de C o buscar tutoriales en línea.

## Ver también

- [Documentación de la función `strftime()` (en inglés)](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Tutorial para convertir fechas en C (en español)](https://es.wikibooks.org/wiki/Programaci%C3%B3n_en_C/Fechas_y_horas)
- [Ejemplos de uso de `strftime()` en diferentes formatos (en inglés)](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)