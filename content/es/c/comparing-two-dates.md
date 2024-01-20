---
title:                "Comparando dos fechas"
html_title:           "C#: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Comparar dos fechas es determinar si una fecha es anterior, posterior o igual a la otra. Los programadores lo hacen para manejar y procesar los datos relacionados con el tiempo, como calcular la duración entre dos fechas.

## Cómo se hace:

Aquí hay un código de ejemplo en C para comparar dos fechas,

```C
#include <stdio.h>

struct Fecha {
    int dia, mes, ano;
};

int comparaFechas(struct Fecha a, struct Fecha b) {
    if(a.ano > b.ano) 
        return 1;
    else if(a.ano < b.ano) 
        return -1;
    else {
        if(a.mes > b.mes) 
            return 1;
        else if(a.mes < b.mes) 
            return -1;
        else {
            if(a.dia > b.dia) 
                return 1;
            else if(a.dia < b.dia) 
                return -1;
            else 
                return 0;
        }
    }
}

int main() {
    struct Fecha f1 = {21, 05, 1998};
    struct Fecha f2 = {3, 12, 2002};

    int comparacion = comparaFechas(f1, f2);
    
    if(comparacion > 0) 
        printf("La fecha 1 es posterior a la fecha 2\n");
    else if(comparacion < 0) 
        printf("La fecha 1 es anterior a la fecha 2\n");
    else 
        printf("Ambas fechas son iguales\n");

    return 0;
}
```
El resultado de este código será "La fecha 1 es anterior a la fecha 2".

## Análisis más detallado:

Historia: La comparación de fechas se remonta a los primeros días de computación, siendo esenciales para varias aplicaciones como la agendación, recordatorios, y más.  

Alternativas: Hay bibliotecas disponibles en C que ofrecen funciones para comparar fechas, como la biblioteca 'time.h'. Estos pueden facilitar el trabajo.

Detalles de implementación: El código anterior utiliza una estructura para almacenar los componentes de una fecha (día, mes y año). Luego compara cada componente en orden de año, mes y día. Devuelve -1 si la primera fecha es anterior, 1 si la primera fecha es posterior y 0 si ambas fechas son iguales.

## Ver También:

Para mas información sobre las fechas en C, revisa estos recursos:

- Trabajando con fechas y horas en C: https://www.cplusplus.com/reference/ctime/
- Biblioteca time.h: http://www.cplusplus.com/reference/ctime/
- Funciones de tiempo en C: https://en.cppreference.com/w/c/chrono