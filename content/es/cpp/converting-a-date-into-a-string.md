---
title:                "C++: Convertir una fecha en un string"
simple_title:         "Convertir una fecha en un string"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una fecha en un string?

Convertir una fecha en un string es una tarea común en la programación. Puede ser útil al crear una aplicación que muestre la fecha actual en un formato legible para los usuarios. También es útil para almacenar y manipular fechas en bases de datos.

## Cómo hacerlo

Para convertir una fecha en un string en C++, se puede utilizar la función strftime() de la librería <ctime>. Esta función toma como argumentos la fecha y hora deseada, junto con el formato en el que se quiere mostrar la fecha.

```C++
#include <iostream>
#include <ctime>

int main() {
    // Obtener la fecha y hora actual
    time_t now = time(0);
    
    // Convertir a un formato legible
    char fecha[100];
    strftime(fecha, 100, "%d/%m/%Y %H:%M:%S", localtime(&now));
    
    // Imprimir el resultado
    cout << "La fecha actual es: " << fecha << endl;
    
    return 0;
}
```

**Output:**
```
La fecha actual es: 28/04/2021 19:45:20
```

También se puede utilizar la función std::stringstream para convertir la fecha en un string. Esta función es especialmente útil si se desea un formato personalizado para la fecha.

```C++
#include <iostream>
#include <sstream>
#include <iomanip>

int main() {
    // Obtener la fecha y hora actual
    time_t now = time(0);
    
    // Crear un objeto std::stringstream
    std::stringstream ss;
    
    // Escribir la fecha en un formato deseado
    ss << std::put_time(localtime(&now), "%B %d, %Y");
    
    // Imprimir el resultado
    std::cout << "La fecha actual es: " << ss.str() << endl;
    
    return 0;
}
```

**Output:**
```
La fecha actual es: April 28, 2021
```

## Profundizando

La función strftime() utiliza una cadena de formato para determinar cómo se mostrará la fecha. Algunos de los símbolos más comúnmente utilizados en esta cadena son:
- "%d": Día del mes (01-31)
- "%m": Mes (01-12)
- "%Y": Año (4 dígitos)
- "%H": Hora en formato de 24 horas (00-23)
- "%M": Minutos (00-59)
- "%S": Segundos (00-59)

También se pueden utilizar otros símbolos para mostrar información adicional, como el día de la semana o la zona horaria.

Otra opción es utilizar la librería <iomanip> para formatear la fecha en un string. Esta librería permite controlar la alineación, el relleno y la precisión de la fecha.

## Ver también

- [Documentación de la función strftime() en cplusplus.com (en inglés)](https://www.cplusplus.com/reference/ctime/strftime/)
- [Referencia de símbolos para la función strftime() en cppreference.com (en inglés)](https://en.cppreference.com/w/cpp/chrono/c/strftime)
- [Tutorial sobre la librería <iomanip> en GeeksforGeeks (en inglés)](https://www.geeksforgeeks.org/iomanip-headers-c-set1/)