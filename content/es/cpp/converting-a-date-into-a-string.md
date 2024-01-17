---
title:                "Convirtiendo una fecha en un string"
html_title:           "C++: Convirtiendo una fecha en un string"
simple_title:         "Convirtiendo una fecha en un string"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Convertir una fecha en una cadena de texto es una técnica utilizada por los programadores para transformar una fecha de formato de fecha y hora a un formato legible para humanos. Esto es útil cuando se desean mostrar fechas en un formato específico o cuando se necesitan procesar fechas en una forma más manejable en el código.

## Cómo:

### Ejemplo 1:

```C++
#include <iostream>
#include <ctime>
using namespace std;

int main() {
    // Obtiene la fecha actual
    time_t now = time(0);
    
    // Convierte la fecha en una cadena de texto
    char* dt = ctime(&now);
    
    // Imprime la fecha en el formato predeterminado
    cout << "La fecha actual es: " << dt << endl;
    
    return 0;
}
```

**Salida:**

```
La fecha actual es: Sat Nov 21 10:23:48 2020
```

### Ejemplo 2:

```C++
#include <iostream>
#include <ctime>
using namespace std;

int main() {
    // Obtiene la fecha actual
    time_t now = time(0);
    
    // Convierte la fecha en una cadena de texto en formato personalizado
    char* dt = strftime("%d/%m/%Y", localtime(&now));
    
    // Imprime la fecha en el formato personalizado
    cout << "La fecha actual es: " << dt << endl;
}
```

**Salida:**

```
La fecha actual es: 21/11/2020
```

## Profundizando:

La conversión de fechas a cadenas de texto se ha vuelto más importante en la programación moderna debido a la necesidad de mostrar fechas en diferentes formatos y en diferentes idiomas. Antes de las bibliotecas estándar de C++, los programadores tenían que implementar sus propias soluciones para convertir fechas en cadenas de texto.

Existen diferentes formas de realizar esta conversión, como utilizando la biblioteca estándar `<ctime>` o la biblioteca de clases `<datetime>`. También es posible utilizar bibliotecas externas, como Boost.DateTime, para realizar conversiones más avanzadas y precisas.

Para convertir fechas en cadenas de texto, es importante tener en cuenta el formato deseado y la zona horaria en la que se quiere mostrar la fecha. Algunas funciones, como `strftime`, permiten especificar el formato deseado, mientras que otras funciones, como `ctime`, utilizan un formato predeterminado.

## Ver también:

- [Biblioteca estándar de C++ - <ctime>](https://en.cppreference.com/w/cpp/chrono/c)
- [Biblioteca de clases <datetime>](https://www.cplusplus.com/reference/datetime/)
- [Boost.DateTime](https://www.boost.org/doc/libs/1_74_0/doc/html/date_time.html)