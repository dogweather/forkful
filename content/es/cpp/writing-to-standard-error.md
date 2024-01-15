---
title:                "Escribiendo en el error estándar"
html_title:           "C++: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

Escribir a la salida de error estándar (también conocida como `stderr`) puede ser útil para depurar y mostrar errores en un programa de C++. También puede ser útil para separar la salida de errores de la salida regular del programa.

## Cómo hacerlo

Para escribir a `stderr` en C++, simplemente utilizamos la función `std::cerr` seguida de los datos que queremos imprimir. Por ejemplo:

```C++
#include <iostream>

int main() {
    int num = 5;
    std::cerr << "Este es un mensaje de error: " << num << std::endl;
    return 0;
}
```

La salida de este programa sería `Este es un mensaje de error: 5` en la salida de error estándar. Nota: `std::endl` se usa para imprimir una nueva línea en `cerr`.

## Profundizando

La salida de error estándar es una forma de salida de datos no formateados. Esto significa que, a diferencia de `std::cout`, no se realiza ningún formato de los datos antes de imprimirlos. Por lo tanto, si queremos imprimir una cadena de caracteres en `cerr`, necesitaríamos especificar la longitud de la cadena para asegurarnos de que se imprima correctamente.

Otra diferencia importante entre `std::cerr` y `std::cout` es que la salida de `cerr` es siempre mostrada, incluso si hay un error en el programa. Esto es muy útil para depurar, ya que podemos ver los mensajes de error sin que el programa finalice.

## Ver también

- Documentación de `std::cerr`: https://www.cplusplus.com/reference/iostream/cerr/
- Guía de depuración en C++: https://www.ibm.com/docs/es/ibm-development-package-cpp/7.0.0?topic=debugging-c-cpp-applications-stderr