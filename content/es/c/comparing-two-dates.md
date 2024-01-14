---
title:    "C: Comparando dos fechas"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué
A menudo, en la programación, es necesario comparar dos fechas para determinar si una es anterior, posterior o igual a la otra. Esto puede ser útil en numerosas aplicaciones, como sistemas de reserva de citas, seguimiento de eventos y tareas, entre otros. La comparación de fechas también puede ser útil en la creación de informes y estadísticas.

## Cómo hacerlo
Para comparar dos fechas en C, primero debemos asegurarnos de que las fechas estén en un formato adecuado. Podemos almacenarlas en variables de tipo estructura ```tm``` y luego utilizar la función ```difftime()``` para calcular la diferencia entre ambas fechas en segundos. Si la diferencia es 0, las fechas son iguales; si es negativa, la primera fecha es anterior a la segunda; y si es positiva, la primera fecha es posterior a la segunda.

```
#include <stdio.h>
#include <time.h>

int main()
{
    struct tm date1 = {0}; // primera fecha
    struct tm date2 = {0}; // segunda fecha

    // establecer valores de fecha y hora
    date1.tm_year = 2021 - 1900;
    date1.tm_mon = 7;
    date1.tm_mday = 10;

    date2.tm_year = 2020 - 1900;
    date2.tm_mon = 11;
    date2.tm_mday = 25;

    // calcular diferencia en segundos
    double diff = difftime(mktime(&date1), mktime(&date2));

    // imprimir resultados
    if (diff == 0)
    {
        printf("Las dos fechas son iguales.\n");
    }
    else if (diff < 0)
    {
        printf("La primera fecha es anterior a la segunda.\n");
    }
    else
    {
        printf("La primera fecha es posterior a la segunda.\n");
    }

    return 0;
}
```

La salida de este programa sería: ```La primera fecha es posterior a la segunda.```

## Profundizando
Ahora que sabemos cómo comparar dos fechas en C, es importante tener en cuenta algunos detalles. Primero, las fechas deben ser válidas y estar en el rango de los valores permitidos para la estructura ```tm```. Además, la función ```difftime()``` devuelve la diferencia en segundos, por lo que podemos utilizarla para comparar no solo fechas, sino también horas y minutos.

También es importante tener en cuenta que la comparación de fechas puede variar según el sistema operativo y la configuración regional en la que se está ejecutando el programa. Por lo tanto, siempre es recomendable hacer pruebas exhaustivas para asegurarse de que el resultado sea el esperado.

## Ver también
- [Documentación de la función difftime en la página de cplusplus.com] (http://www.cplusplus.com/reference/ctime/difftime/)
- [Tutorial sobre estructuras en C] (https://www.learn-c.org/en/C-Structures)
- [Preguntas frecuentes sobre las estructuras de tiempo en C] (https://www.tutorialspoint.com/c_standard_library/time_h.htm)