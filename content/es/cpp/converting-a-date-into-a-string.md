---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué es y Por qué?

Convertir una fecha en una cadena en C++ significa representar una fecha como texto, en lugar de tenerla almacenada en un formato numérico o de fecha/hora. Los programadores hacen esto por legibilidad y para facilitar la manipulación y el formato de fechas, como la presentación de fechas en formas específicas para diferentes regiones del mundo.

## ¿Cómo hacerlo?

Es simple usar la biblioteca `<chrono>` y la biblioteca `<sstream>` para este propósito en C++.

```C++
#include <chrono>
#include <sstream>

std::string getDate(){
    auto ahora = std::chrono::system_clock::now();
    std::time_t tiempo = std::chrono::system_clock::to_time_t(ahora);
    std::tm* ptm = std::localtime(&tiempo);
    std::stringstream fecha;
    fecha << (ptm->tm_year+1900) << "/" // año 
          << (ptm->tm_mon+1) << "/" // mes
          <<  ptm->tm_mday;  // día
    return fecha.str();
}
```
Al ejecutar `getDate()`, obtendrías una salida como `2022/03/04`.

## Profundización

### Contexto histórico
El formato de fecha-a-cadena ha sido un componente esencial para aumentar la legibilidad en la programación desde sus inicios. Antes de C++11, los programadores usaban la biblioteca `<ctime>` para realizar esta tarea, que ahora se realiza principalmente con `<chrono>` y `<stringstream>`.

### Alternativas
Otras bibliotecas, como Boost DateTime, pueden proporcionar un enfoque más rico y alternativo para convertir la fecha en cadena.

### Detalles de implementación
En el código anterior, se utiliza `<chrono>` para obtener la hora del sistema en milisegundos desde la época y se convierte en `time_t`. Luego, convertimos `time_t` en `tm` (estructura de tiempo) usando `localtime`. Finalmente, usamos `stringstream` para formatear y almacenar la fecha en una variable de cadena.

## Ver También

Para profundizar en el tema, puedes consultar:
- [cppreference - std::chrono](https://en.cppreference.com/w/cpp/chrono)
- [cppreference - std::stringstream](https://en.cppreference.com/w/cpp/io/basic_stringstream)
- [Boost DateTime documentation](https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html) for more complexity and variety in dealing with dates and times in C++.