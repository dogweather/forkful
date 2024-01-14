---
title:    "C++: Leyendo argumentos de línea de comandos."
keywords: ["C++"]
---

{{< edit_this_page >}}

# ¿Por qué deberías leer argumentos de línea de comandos en C++?

Si eres un programador de C++, es importante entender cómo leer y manejar argumentos de línea de comandos en tus programas. Esto te permitirá crear programas más interactivos y versátiles, ya que podrás recibir información del usuario directamente desde la línea de comandos.

# Cómo hacerlo:

Para leer argumentos de línea de comandos en C++, primero debes incluir la biblioteca `cstdlib` en tu código. Luego, puedes utilizar la función `atoi` para convertir los argumentos ingresados en la línea de comandos a valores numéricos, como se muestra en el siguiente ejemplo:

```C++
#include <cstdlib>
#include <iostream>

int main(int argc, char* argv[]) {
  // Verifica si se ingresaron argumentos en la línea de comandos
  if (argc > 1) {
    // Convierte el argumento ingresado a un entero utilizando la función atoi
    int argumento = std::atoi(argv[1]);

    // Imprime el argumento y su doble
    std::cout << "El argumento ingresado fue: " << argumento << std::endl;
    std::cout << "Su doble es: " << argumento * 2 << std::endl;
  }
  return 0;
}
```

Si compilamos y ejecutamos este código con el argumento `5` en la línea de comandos, obtendremos la siguiente salida:

```
El argumento ingresado fue: 5
Su doble es: 10
```

# Profundizando más:

Además de la función `atoi`, existen otras formas de leer y manejar argumentos de línea de comandos en C++. La biblioteca `getopt` permite analizar y obtener los argumentos de manera más estructurada, mientras que la biblioteca `boost::program_options` ofrece más opciones y funcionalidades avanzadas.

También es importante tener en cuenta que los argumentos de línea de comandos pueden ser muy útiles en la depuración y pruebas de nuestras aplicaciones, ya que nos permiten ingresar valores directamente en tiempo de ejecución.

# Ver también:

- [Biblioteca cstdlib en C++ (en inglés)](https://www.cplusplus.com/reference/cstdlib/)
- [Ejemplo de uso de getopt en C++ (en inglés)](https://www.gnu.org/software/libc/manual/html_node/Example-of-Getopt.html)
- [Documentación de boost::program_options en C++ (en inglés)](https://www.boost.org/doc/libs/1_74_0/doc/html/program_options.html)