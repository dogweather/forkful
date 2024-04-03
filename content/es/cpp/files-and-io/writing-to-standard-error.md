---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:34.625519-07:00
description: "C\xF3mo hacerlo: En C++, escribir en el error est\xE1ndar se puede lograr\
  \ utilizando el flujo `cerr`, que es parte de la biblioteca est\xE1ndar. Aqu\xED\
  \ hay un\u2026"
lastmod: '2024-03-13T22:44:59.391256-06:00'
model: gpt-4-0125-preview
summary: "En C++, escribir en el error est\xE1ndar se puede lograr utilizando el flujo\
  \ `cerr`, que es parte de la biblioteca est\xE1ndar."
title: "Escribiendo en el error est\xE1ndar"
weight: 25
---

## Cómo hacerlo:
En C++, escribir en el error estándar se puede lograr utilizando el flujo `cerr`, que es parte de la biblioteca estándar. Aquí hay un ejemplo básico:

```cpp
#include <iostream>

int main() {
    // Escribiendo en la salida estándar
    std::cout << "Este es un mensaje normal." << std::endl;
    
    // Escribiendo en el error estándar
    std::cerr << "Este es un mensaje de error." << std::endl;
    
    return 0;
}
```

Salida de muestra:
```
Este es un mensaje normal.
Este es un mensaje de error.
```

En este caso, ambos mensajes típicamente aparecerán en tu terminal, pero puedes redirigirlos por separado en un shell. Por ejemplo, puedes enviar la salida estándar a un archivo mientras permites que los errores se muestren en la pantalla.

Para un registro y manejo de errores más avanzado, se pueden emplear bibliotecas de terceros como `spdlog` o `boost.log`. Estas bibliotecas ofrecen características mejoradas para el registro, incluyendo formateo, niveles de registro y salida de archivo.

Así es como podrías usar `spdlog` para escribir un mensaje de error:

```cpp
#include "spdlog/spdlog.h"

int main() {
    // Inicializando spdlog
    spdlog::info("Este es un mensaje normal.");
    spdlog::error("Este es un mensaje de error.");
    
    return 0;
}
```

Nota: Para usar `spdlog`, necesitas añadirlo a tu proyecto. Puedes hacer esto clonando el repositorio de GitHub o usando un administrador de paquetes como `vcpkg` o `conan`.

Recuerda, la elección entre usar flujos estándar directamente o una biblioteca como `spdlog` depende de la complejidad de tu aplicación y tus necesidades específicas en cuanto a manejo de errores y registro.
