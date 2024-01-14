---
title:                "C++: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Por qué extraer subcadenas en C++?

Extraer subcadenas (también conocidas como subcadenas o cadenas secundarias) es una técnica útil en programación para obtener una sección específica de una cadena de texto. Esto puede ser importante en situaciones donde solo se necesita una parte de una cadena más grande, como buscar una palabra clave o procesar datos en un formato específico. Aprender a extraer subcadenas en C++ puede ayudar a mejorar la eficiencia y la precisión de su código.

## Cómo extraer subcadenas en C++

Para extraer una subcadena en C++, utilizamos la función `substr()` de la biblioteca `string`. Esta función toma dos argumentos: la posición inicial de la subcadena y la longitud deseada. Por ejemplo, si queremos extraer la subcadena "hola" de la cadena "¡Hola mundo!", la función se vería así:

```C++
string texto = "¡Hola mundo!";
string subcadena = texto.substr(1, 4);
cout << subcadena; // Salida: "hola"
```
Tenga en cuenta que la posición inicial comienza en 0 y la longitud incluye el último carácter de la subcadena. También podemos usar variables en lugar de valores fijos para los argumentos, lo que nos permite extraer subcadenas de manera más dinámica.

## Profundizando en la extracción de subcadenas

Además de la función `substr()`, también podemos utilizar la función `find()` para encontrar la posición de una subcadena dentro de otra cadena de texto. Y si necesitamos extraer múltiples subcadenas, podemos utilizar un bucle y la función `find()` para iterar a través de la cadena y extraerlas una por una. También existen otras técnicas para la extracción de subcadenas, como el uso de expresiones regulares.

## Ver también
- [Documentación de `substr()` en cplusplus.com](https://www.cplusplus.com/reference/string/string/substr/)
- [Tutorial de extracción de subcadenas en C++ en hackr.io](https://hackr.io/tutorials/learn-c-plus-plus/substring-in-c)