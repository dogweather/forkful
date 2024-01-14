---
title:                "C++: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué

La comparación de fechas es una tarea común en la programación de C++, ya sea para ordenar eventos en secuencia cronológica o para realizar cálculos basados en intervalos de tiempo. Aprender a comparar dos fechas te ayudará a mejorar tus habilidades de programación y hacer tu código más eficiente.

## Cómo hacerlo

Para comparar dos fechas en C++, puedes utilizar la función `difftime()` de la biblioteca `<ctime>`.

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
    // Definimos dos estructuras de tiempo para las fechas que queremos comparar
    struct tm t1 = {0};
    struct tm t2 = {0};

    // Asignamos valores a las estructuras de tiempo
    t1.tm_year = 2019 - 1900;
    t1.tm_mon = 4 - 1;
    t1.tm_mday = 1;

    t2.tm_year = 2019 - 1900;
    t2.tm_mon = 5 - 1;
    t2.tm_mday = 1;
    
    // Convertimos las estructuras de tiempo a variables de tipo tiempo
    // utilizando la función mktime()
    time_t fecha1 = mktime(&t1);
    time_t fecha2 = mktime(&t2);

    // Calculamos la diferencia en segundos entre las dos fechas
    double diffSec = difftime(fecha2, fecha1);
    cout << "La diferencia en segundos es: " << diffSec << endl;
    
    return 0;
}
```

El resultado de este código sería:

```
La diferencia en segundos es: 2592000
```

Puedes utilizar la función `difftime()` para calcular la diferencia en otros intervalos de tiempo, como minutos, horas, días, etc.

## Profundizando

La función `difftime()` devuelve un valor de tipo `double` que representa la diferencia en segundos entre las dos fechas. Esta función toma en cuenta factores como los años bisiestos y los husos horarios, lo que la hace muy precisa para realizar cálculos de tiempo.

Además de la función `difftime()`, también puedes utilizar la función `compare()` de la biblioteca `<chrono>` para comparar dos fechas en C++, con resultados más detallados.

## Ver también

- Documentación de `difftime()`: https://www.cplusplus.com/reference/ctime/difftime/
- Documentación de `compare()`: https://en.cppreference.com/w/cpp/chrono/c/neq