---
title:                "Convirtiendo una fecha en una cadena de texto."
html_title:           "C++: Convirtiendo una fecha en una cadena de texto."
simple_title:         "Convirtiendo una fecha en una cadena de texto."
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué convertir una fecha en una cadena de texto

Convertir una fecha en una cadena de texto es una tarea común en la programación. Puede ser necesario para mostrar la fecha en un formato específico, para almacenarla en una base de datos o para realizar cálculos con ella. A continuación, se explicará cómo hacerlo de manera sencilla en C++.

## Cómo hacerlo

La conversión de una fecha en una cadena de texto se puede realizar utilizando la función `strftime`. Esta función toma como parámetros la fecha que queremos convertir y un formato de cadena que especifica cómo se debe mostrar la fecha.

Por ejemplo, si queremos mostrar la fecha actual en el formato "DD/MM/AAAA" (día/mes/año), podemos utilizar el siguiente código:

```C++
#include <iostream>
#include <ctime>

int main() {
    // Obtener la fecha actual
    time_t now = time(0);
    tm *timeinfo = localtime(&now);

    // Convertir la fecha en una cadena de texto
    char fecha[11]; // se reserva espacio para 11 caracteres
    strftime(fecha, 11, "%d/%m/%Y", timeinfo);

    // Mostrar la fecha en la consola
    std::cout << "La fecha de hoy es: " << fecha << std::endl;

    return 0;
}
```

El resultado de este código sería:

```
La fecha de hoy es: 20/08/2021
```

## Profundizando

La función `strftime` se basa en códigos de formato para especificar cómo se debe mostrar la fecha. Algunos de los más utilizados son los siguientes:

- `%Y` para el año completo (ejemplo: 2021)
- `%y` para el año en dos dígitos (ejemplo: 21)
- `%m` para el mes con ceros a la izquierda (ejemplo: 08)
- `%B` para el nombre completo del mes (ejemplo: agosto)
- `%d` para el día con ceros a la izquierda (ejemplo: 20)
- `%A` para el día de la semana completo (ejemplo: viernes)

Puedes encontrar una lista completa de los códigos de formato y sus respectivos resultados en la <a href="https://en.cppreference.com/w/cpp/chrono/c/strftime" target="_blank">documentación de C++</a>.

## Ver también

- <a href="https://www.cplusplus.com/reference/ctime/strftime/" target="_blank">Documentación de la función `strftime` en C++</a>
- <a href="https://www.w3schools.com/cpp/cpp_date.asp" target="_blank">Tutorial sobre fechas en C++ en W3Schools</a>
- <a href="https://www.geeksforgeeks.org/converting-strings-numbers-cc/" target="_blank">Cómo convertir números a cadenas de texto en C++</a>