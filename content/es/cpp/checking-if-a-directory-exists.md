---
title:    "C++: Verificar si existe un directorio."
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por qué

En programación, es común tener la necesidad de verificar si un directorio existe antes de realizar alguna operación. Esto nos permite asegurarnos de que nuestro código funcione de manera adecuada, evitando errores y posibles situaciones de falla.

## Cómo hacerlo

La forma más fácil de verificar si un directorio existe en C ++ es utilizando la función "opendir" de la biblioteca estándar. Esta función toma como parámetro la ruta del directorio y devuelve un puntero al directorio si existe, o un valor nulo si no existe.

```C++
DIR* dir = opendir("ruta/del/directorio");

if(dir) {
    std::cout << "¡El directorio existe!";
} else {
    std::cout << "El directorio no existe";
}

closedir(dir);
```

El código anterior primero intenta abrir el directorio utilizando la función "opendir". Si el directorio existe, se asigna un puntero al mismo a la variable "dir", lo que nos permite imprimir un mensaje indicando que el directorio existe. Si el directorio no existe, la función devolverá un valor nulo y se imprimirá un mensaje indicando que no existe.

También podemos aprovechar la función "stat" para verificar la existencia de un directorio. Esta función toma como parámetro la ruta del directorio y una estructura "stat" que se utilizará para almacenar información sobre el mismo. Si la función tiene éxito en encontrar información sobre el directorio, podemos utilizar el campo "st_mode" para determinar si es un directorio o no.

```C++
struct stat info;

if(stat("ruta/del/directorio", &info) == 0 && S_ISDIR(info.st_mode)) {
    std::cout << "¡El directorio existe!";
} else {
    std::cout << "El directorio no existe";
}
```

En este caso, utilizamos la función "S_ISDIR" para verificar si el campo "st_mode" de la estructura "stat" indica que el archivo es un directorio o no.

## Profundizando

Además de las funciones mencionadas, existen otras formas de verificar la existencia de un directorio en C ++. Por ejemplo, podemos utilizar la biblioteca Boost y su función "exists" para comprobar si un directorio existe.

También es importante tener en cuenta que, aunque podamos verificar la existencia de un directorio, esto no significa que tengamos permisos para acceder a él. Por lo tanto, es importante manejar correctamente estas situaciones y asegurarse de tener los permisos necesarios antes de intentar realizar operaciones en un directorio.

## Ver también

- [Referencia de la función opendir de C ++](https://www.cplusplus.com/reference/cstdio/opendir/)
- [Utilizando la función stat en C ++](https://www.tutorialspoint.com/c_standard_library/c_function_stat.htm)
- [Documentación de la biblioteca Boost](https://www.boost.org/)