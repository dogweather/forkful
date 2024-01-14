---
title:                "C++: Leyendo argumentos de línea de comando"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué leer argumentos de línea de comando?

Lee este blog post si estás buscando aprender cómo utilizar argumentos de línea de comando en tus programas de C++. La utilización de argumentos de línea de comando puede hacer que tus programas sean más flexibles y dinámicos, permitiéndote personalizar la ejecución del programa según tus necesidades.

## Cómo hacerlo

Para leer argumentos de línea de comando en C++, primero debes incluir la biblioteca `iostream` y `string` en tu programa. A continuación, utiliza la función `int main(int argc, char* argv[])` para leer los argumentos de línea de comando. El parámetro `argc` contendrá el número de argumentos ingresados y `argv` será un array con los argumentos.

Aquí tienes un ejemplo de cómo puedes utilizar argumentos de línea de comando para imprimir un mensaje personalizado en la consola:

```C++
#include <iostream>
#include <string>

int main(int argc, char* argv[]) {
  if (argc > 1) {
    std::string nombre = argv[1];
    std::cout << "Hola " << nombre << ", bienvenido/a a mi programa!";
  } else {
    std::cout << "Bienvenido al programa!";
  }
  return 0;
}
```

Si ejecutas el programa anterior en la línea de comando ingresando `./programa.exe Juan`, obtendrás el siguiente resultado:

```
Hola Juan, bienvenido/a a mi programa!
```

Si no ingresas ningún argumento, el programa imprimirá:

```
Bienvenido al programa!
```

## Profundizando

Ahora que sabes cómo leer argumentos de línea de comando, es importante tener en cuenta algunos detalles adicionales. El primer argumento ingresado (`argv[0]`) siempre será el nombre del programa en ejecución. Por lo tanto, si necesitas acceder únicamente a los argumentos ingresados por el usuario, debes empezar a recorrer el array a partir de `argv[1]`.

Otro aspecto importante es que los argumentos de línea de comando siempre se leerán como cadenas de texto, incluso si son números o caracteres especiales. Si necesitas convertir los argumentos a un tipo de datos específico, como un `int` o un `char`, deberás hacerlo utilizando funciones de conversión.

## Ver también

- [Documentación oficial de C++ sobre la función `main`](https://es.cppreference.com/w/cpp/language/main_function)
- [Tutorial de DevC++ sobre argumentos de línea de comando](https://www.devdungeon.com/content/using-command-line-arguments-paper-c)
- [Tutorial de Programiz sobre argumentos de línea de comando en C++](https://www.programiz.com/cpp-programming/main-function-argc-argv)