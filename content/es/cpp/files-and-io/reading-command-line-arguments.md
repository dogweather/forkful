---
date: 2024-01-20 17:55:39.200202-07:00
description: "Leer argumentos de la l\xEDnea de comandos permite que tu programa en\
  \ C++ sea flexible y reciba datos externos al iniciarse. Los programamos as\xED\
  \ para\u2026"
lastmod: '2024-03-13T22:44:59.390278-06:00'
model: gpt-4-1106-preview
summary: "Leer argumentos de la l\xEDnea de comandos permite que tu programa en C++\
  \ sea flexible y reciba datos externos al iniciarse."
title: "Lectura de argumentos de l\xEDnea de comandos"
weight: 23
---

## ¿Qué y Por Qué?

Leer argumentos de la línea de comandos permite que tu programa en C++ sea flexible y reciba datos externos al iniciarse. Los programamos así para interactuar con el usuario o automatizar tareas con diferentes parámetros sin cambiar el código.

## Cómo:

Aquí tienes cómo capturar argumentos de la línea de comandos en C++:

```C++
#include <iostream>
int main(int argc, char *argv[]) {
    std::cout << "Has introducido " << argc << " argumentos:" << std::endl;
    
    for (int i = 0; i < argc; ++i) {
        std::cout << i << ": " << argv[i] << std::endl;
    }

    return 0;
}
```

Si compilas y ejecutas este programa así `./mi_programa hola mundo`, verás:

```plaintext
Has introducido 3 argumentos:
0: ./mi_programa
1: hola
2: mundo
```

## Análisis Profundo:

La convención de pasar argumentos data de los primeros días de las interfaces de línea de comandos. En UNIX y sistemas similares, `argc` representa la cantidad de argumentos y `argv` es un array de cadenas (char*) que contiene los propios argumentos. 

Alternativas a la manipulación manual de `argv` incluyen librerías como `getopt`, para opciones más complejas, o bibliotecas de análisis de argumentos de modernas como `Boost.Program_options` y `TCLAP`. Detrás de escena, el sistema operativo pasa los argumentos a `main` cuando se lanza el programa.

Implementaciones variadas pueden cambiar ligeramente entre plataformas y compiladores. Por ejemplo, en Windows, también puedes usar `GetCommandLine()` de la API de Windows para acceder a la línea de comandos completa como una sola cadena.

## Ver También:

- Tutorial de C++ sobre la librería Boost.Program_options: http://www.boost.org/doc/libs/release/doc/html/program_options.html
- Documentación de TCLAP - Templatized C++ Command Line Parser Library: http://tclap.sourceforge.net/
- Artículo de Wikipedia sobre la interfaz de línea de comandos: https://es.wikipedia.org/wiki/Interfaz_de_l%C3%ADnea_de_comandos
