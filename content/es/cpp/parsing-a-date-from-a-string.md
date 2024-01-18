---
title:                "Analizando una fecha de una cadena"
html_title:           "C++: Analizando una fecha de una cadena"
simple_title:         "Analizando una fecha de una cadena"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parsing una fecha de una cadena en C++

## ¿Qué y por qué?

Parsing una fecha de una cadena se refiere a la tarea de extraer información de una cadena de texto que representa una fecha y convertirla en un formato de fecha reconocido por la computadora. Los programadores a menudo tienen que realizar esta tarea para procesar datos o validar la entrada de usuario en programas que involucran fechas.

## Cómo hacerlo:

La forma más común de lograr esto en C++ es mediante el uso de la librería estándar de C++ ```std::stringstream```. A continuación se muestra un ejemplo de cómo se puede usar para analizar una fecha:

```C++
#include <iostream>
#include <string>
#include <sstream>

int main() {
  // Definir la cadena de texto que contiene la fecha
  std::string fecha = "10/31/2021";

  // Crear un objeto stringstream para analizar la fecha
  std::stringstream ss(fecha);

  int dia, mes, anio;

  // Extraer las componentes de la fecha de la cadena
  ss >> mes;
  ss.ignore(); // Ignorar el '/' entre el mes y el día
  ss >> dia;
  ss.ignore(); // Ignorar el '/' entre el día y el año
  ss >> anio;

  // Imprimir la fecha en un formato reconocido por la computadora
  std::cout << "La fecha es: " << anio << "/" << mes << "/" << dia << std::endl;

  return 0;
}
```

La salida de este programa sería: 
```
La fecha es: 2021/10/31
```

## Detalles técnicos:

Además del método anterior, existen otras formas en las que los programadores pueden implementar el parsing de una fecha. Algunas alternativas incluyen el uso de expresiones regulares o librerías externas dedicadas a manejar fechas. Sin embargo, la librería estándar de C++ suele ser suficiente para cumplir con la mayoría de las necesidades.

## Referencias:

- [Documentación de std::stringstream en cppreference.com](https://en.cppreference.com/w/cpp/io/basic_stringstream)
- [Expresiones regulares - referencia de la librería estándar de C++](https://en.cppreference.com/w/cpp/regex)
- [Librería Chrono para manejo de fechas en C++](https://en.cppreference.com/w/cpp/chrono)

**¡Esperamos que esta guía sobre cómo hacer parsing de una fecha en C++ te haya sido útil!**