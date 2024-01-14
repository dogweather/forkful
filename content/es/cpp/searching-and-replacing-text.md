---
title:    "C++: Buscando y reemplazando texto"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

##Por qué

La búsqueda y reemplazo de texto es una tarea común en la programación. Ya sea que estés haciendo cambios en un archivo de texto o en un programa completo, la función de búsqueda y reemplazo te permitirá hacer cambios rápidos y eficientes. Además, es una habilidad útil para tener en tu arsenal de programación.

##Cómo hacerlo

La búsqueda y reemplazo en C++ se pueden realizar de diferentes formas, pero aquí te mostraremos una manera sencilla de hacerlo utilizando la función `find` y `replace` de la librería `string`.

Primero, necesitaremos incluir la librería `string` en nuestro código:

```C++
#include <string>
```

Luego, podemos utilizar la función `find` para buscar una cadena de texto dentro de otra. Por ejemplo, tenemos la siguiente cadena:

```C++
std::string texto = "¡Hola Mundo!";
```

Si queremos buscar y reemplazar la palabra "Mundo" por "Amigos", podemos hacerlo de la siguiente manera:

```C++
std::string palabra_buscar = "Mundo";
std::string palabra_reemplazar = "Amigos";

size_t indice = texto.find(palabra_buscar);
texto.replace(indice, palabra_buscar.length(), palabra_reemplazar);

std::cout << texto << std::endl;
```

La función `find` nos devuelve el índice donde se encuentra la cadena buscada, y luego utilizamos la función `replace` para reemplazar la palabra en ese índice con la nueva palabra.

El resultado final en la consola sería:

```
¡Hola Amigos!
```

##Profundizando

Además de la función `find` y `replace`, la librería `string` también cuenta con otras funciones útiles para la búsqueda y reemplazo de texto, como por ejemplo `find_first_of`, `find_last_of` y `replace_if`.

También puedes utilizar expresiones regulares para realizar búsquedas más complejas. La librería `regex` de C++ te permite utilizar patrones para buscar y reemplazar cadenas de texto.

Para profundizar más en el tema, te recomendamos revisar la documentación oficial de C++ y explorar diferentes ejemplos en línea.

##Ver también

- [Documentación oficial de C++ (en español)](https://es.cppreference.com/w/)
- [Tutorial de búsqueda y reemplazo en C++ (en inglés)](https://www.geeksforgeeks.org/search-replace-cpp/)
- [Ejemplos de expresiones regulares en C++ (en inglés)](https://www.regular-expressions.info/examples.html)