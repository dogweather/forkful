---
title:                "Escribiendo un archivo de texto"
html_title:           "C++: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

¡Hola amigos! Si estás leyendo esto, es probablemente porque quieres aprender cómo escribir un archivo de texto en C++. Y te felicito por eso, porque escribir archivos de texto es una habilidad esencial para cualquier programador. ¿Por qué? Porque los archivos de texto nos permiten almacenar y acceder a datos de manera estructurada y legible para los seres humanos. Además, son útiles para guardar configuraciones o crear registros en nuestras aplicaciones. Así que, ¡vamos a aprender!

## Cómo hacerlo

Para escribir un archivo de texto en C++, necesitas seguir tres pasos simples:

1. Abrir el archivo utilizando la función `ofstream`.
2. Escribir en el archivo utilizando el operador `<<` y los datos que deseamos almacenar.
3. Cerrar el archivo utilizando la función `close()`.

Veámoslo en acción con un ejemplo básico:

```C++
#include <fstream>

int main() {
    // Abrimos el archivo "datos.txt" en modo escritura
    std::ofstream archivo("datos.txt");

    // Escribimos en el archivo
    archivo << "¡Hola! Este es un archivo de texto escrito con C++.";

    // Cerramos el archivo
    archivo.close();

    return 0;
}
```

Fácil, ¿verdad? Ahora, si abrimos el archivo "datos.txt", veremos que contiene nuestro mensaje guardado. Pero, ¿qué pasa si queremos escribir varios datos, como números o variables? No te preocupes, también podemos hacerlo con facilidad. Echa un vistazo a este otro ejemplo:

```C++
#include <fstream>

int main() {
    // Abrimos el archivo "numeros.txt" en modo escritura
    std::ofstream archivo("numeros.txt");

    // Creamos algunas variables con datos numéricos
    int edad = 25;
    float altura = 1.75;
    double peso = 70.5;

    // Escribimos en el archivo utilizando las variables
    archivo << "Edad: " << edad << std::endl;
    archivo << "Altura: " << altura << " metros" << std::endl;
    archivo << "Peso: " << peso << " kg";

    // Cerramos el archivo
    archivo.close();

    return 0;
}
```

¡Wow! Ahora hemos escrito no solo un, sino tres datos en nuestro archivo de texto. Y hemos utilizado diferentes tipos de datos para demostrar que podemos escribir cualquier tipo de información que necesitemos. Recuerda utilizar la función `endl` para cambiar de línea en el archivo y que el operador `<<` es nuestro aliado para escribir diferentes valores.

## Inmersión profunda

Ahora que sabemos cómo escribir un archivo de texto en C++, podemos profundizar un poco más y aprender algunas cosas interesantes. Primero, debes tener en cuenta que puedes especificar la ruta del archivo que estás escribiendo. Por ejemplo, si quieres escribir en una carpeta diferente a la que contiene el archivo ejecutable de tu programa, puedes hacerlo agregando la ruta al nombre del archivo, como en este ejemplo:

```C++
// Escribimos en el archivo "output.txt" ubicado en la carpeta "resultados"
std::ofstream archivo("resultados/output.txt");
```

Además, si queremos agregar información al final de un archivo existente en lugar de sobrescribirlo, podemos abrir el archivo en modo de apertura inmediata (`app`), como en este ejemplo:

```C++
// Agregamos información al final del archivo sin sobrescribirlo
std::ofstream archivo("datos.txt", std::ios::app);
```

Por último, pero no menos importante, podemos crear estructuras de datos más complejas y escribirlas en un archivo de texto utilizando la función `write()`. Esta función nos permite escribir un conjunto de bytes en el archivo, lo que nos da más flexibilidad en términos de cómo estructuramos y almacenamos nuestros datos. Pero esa es una historia para otro día.

## Ver también

- [Documentación de C++ sobre escritura de archivos](https://www.cplusplus.com/doc/tutorial/files/#writing_files)
- [Más ejemplos de escritura de archivos en C++](https://www.geeksforgeeks.org/write-text-file-c/)
- [Guía de Markdown para dar estilo a tus archivos de texto](https://www.markdownguide.org/basic-syntax/)