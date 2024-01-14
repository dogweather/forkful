---
title:    "C++: Convirtiendo una fecha en una cadena de caracteres"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##Por qué

Convertir una fecha en una cadena de texto puede ser útil en varias situaciones, como por ejemplo, imprimir una fecha en un formato específico o almacenarla en una base de datos como una cadena de caracteres. En este artículo, aprenderemos cómo convertir una fecha en una cadena de texto utilizando C++.

##Cómo hacerlo

Para convertir una fecha en una cadena de texto en C++, necesitaremos utilizar la biblioteca estándar de C++ <ctime>, que contiene funciones para trabajar con fechas y horas. También utilizaremos la función `strftime()` para realizar la conversión.

Primero, debemos incluir la biblioteca <ctime> en nuestro código:

```C++
#include <ctime>
```

Luego, debemos definir una estructura `tm` que representa una fecha y hora específicas. Esta estructura tiene los siguientes miembros:

- `tm_sec` para los segundos (de 0 a 60)
- `tm_min` para los minutos (de 0 a 59)
- `tm_hour` para las horas (de 0 a 23)
- `tm_mday` para el día del mes (de 1 a 31)
- `tm_mon` para el mes (de 0 a 11)
- `tm_year` para el año (desde 1900)
- `tm_wday` para el día de la semana (de 0 a 6)
- `tm_yday` para el día del año (de 0 a 365)
- `tm_isdst` para indicar si el horario de verano está en efecto

A continuación, debemos crear una variable de tipo `tm` y asignar los valores correspondientes para la fecha que queremos convertir en una cadena de texto. Por ejemplo, si queremos convertir la fecha 15 de agosto de 2021, nuestra variable `tm` se vería así:

```C++
struct tm fecha;
fecha.tm_sec = 0;
fecha.tm_min = 0;
fecha.tm_hour = 0;
fecha.tm_mday = 15;
fecha.tm_mon = 7;
fecha.tm_year = 2021 - 1900;
fecha.tm_wday = 0;
fecha.tm_yday = 0;
fecha.tm_isdst = -1;
```

Finalmente, utilizaremos la función `strftime()` para convertir nuestra variable `tm` en una cadena de texto. Esta función toma tres parámetros: un búfer para almacenar la cadena de texto resultante, su tamaño y un formato que especifica cómo queremos que se muestre la fecha convertida. El formato es una cadena de texto que contiene ciertos caracteres especiales que representan diferentes partes de la fecha. Por ejemplo, `%d` representa el día del mes con ceros a la izquierda, mientras que `%Y` representa el año con cuatro dígitos.

Aquí hay un ejemplo completo de cómo convertir la fecha del 15 de agosto de 2021 en una cadena de texto en formato "DD/MM/YYYY":

```C++
#include <iostream>
#include <ctime>

int main() {
    struct tm fecha;
    fecha.tm_sec = 0;
    fecha.tm_min = 0;
    fecha.tm_hour = 0;
    fecha.tm_mday = 15;
    fecha.tm_mon = 7;
    fecha.tm_year = 2021 - 1900;
    fecha.tm_wday = 0;
    fecha.tm_yday = 0;
    fecha.tm_isdst = -1;

    char buffer[11]; // búfer para almacenar la cadena de texto resultante
    strftime(buffer, sizeof(buffer), "%d/%m/%Y", &fecha); // formato DD/MM/YYYY
    std::cout << buffer << std::endl; // imprimir la cadena de texto resultante

    return 0;
}
```

El resultado de la ejecución de este código sería "15/08/2021".

##Profundizando

La función `strftime()` también puede ser utilizada para mostrar la hora, no solo la fecha. Además, hay una gran cantidad de caracteres especiales que se pueden utilizar en el formato para mostrar diferentes aspectos de la fecha y la hora, como el número de semana, el nombre del mes o incluso la hora en formato de 24 horas. Puedes encontrar una lista completa de estos caracteres especiales en la documentación de <ctime> de C++. También puedes utilizar la función `time()` para obtener la fecha y hora actual y convertirla en una cadena de texto.

##Ver también

- <ctime> documentación de C++: http://www.cplusplus.com/reference/ctime/
- strftime() documentación de C++: http://www.cplusplus.com