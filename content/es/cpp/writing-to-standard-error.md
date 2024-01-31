---
title:                "Escribiendo en el error estándar"
date:                  2024-01-19
html_title:           "Arduino: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir en el error estándar (stderr) permite mostrar errores y diagnósticos importantes, separándolos de la salida normal (stdout). Los programadores usan stderr para asegurarse de que los mensajes de error sean visibles y no se confundan con otros datos de salida.

## Cómo:
Aquí tienes un ejemplo sencillo de cómo escribir en `stderr`:

```C++
#include <iostream>

int main() {
    std::cerr << "Ha ocurrido un error inesperado." << std::endl;
    return 1;
}
```

Si ejecutas este programa, verás el mensaje de error en tu consola.

## Profundizando:
Históricamente, la separación de stdout y stderr viene de la época de Unix, donde era útil diferenciar entre datos regulares y mensajes de diagnóstico. Alternativas incluyen el uso de logging frameworks, que ofrecen más control y opciones. Stderr es implementado en C++ a través de la librería iostream y es global y thread-safe por defecto.

## Ver También:
- Documentación de `<iostream>`: https://en.cppreference.com/w/cpp/header/iostream
- Guía sobre manejo de errores en C++: https://www.cplusplus.com/doc/tutorial/exceptions/
- Explicación del diseño de los streams estándar de Unix: https://unix.stackexchange.com/questions/331611/why-were-stderr-and-stdout-created-separately
