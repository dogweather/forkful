---
title:                "Buscando y reemplazando texto"
html_title:           "C: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Buscar y reemplazar texto es simplemente ubicar una secuencia de caracteres en un conjunto más grande de caracteres y luego reemplazarla por otra. Los programadores lo hacen para manipular y transformar los datos del texto con eficacia.

## ¿Cómo hacerlo?

Aquí está el código que muestra cómo buscar y reemplazar texto en C++. Toma en cuenta que se necesita la biblioteca estándar de cadena para hacer esto. 

```C++
#include <iostream>
#include <string>

int main(){
    std::string texto = "Me encanta programar en Java.";
    std::string buscar = "Java";
    std::string reemplazar = "C++";
    
    size_t pos = texto.find(buscar);
    if(pos != std::string::npos){
        texto.replace(pos, buscar.size(), reemplazar);
    }
    std::cout << texto << std::endl;
}
```

Cuando ejecutamos este código, la salida será:

```C++
"Me encanta programar en C++."
```

## Inmersión Profunda

Historicamente, la posibilidad de buscar y reemplazar texto es una parte integral de la programación. Permitió a los programadores manipular rápidamente y con precisión vastos volúmenes de datos. Las alternativas a esto varían entre los lenguajes de programación. Algunos como Python permiten esto directamente con funciones predefinidas, mientras que otros como Java requieren importar bibliotecas específicas o definir las funciones de uno mismo. 

En C++, podemos utilizar la biblioteca de cadena estándar, que proporciona un método `replace`, pero la implementación interna depende del compilador. Generalmente, extrae la subcadena, la elimina y luego inserta la nueva.

## Ver También

- [cplusplus.com - std::string::replace](http://www.cplusplus.com/reference/string/string/replace/)
- [cplusplus.com - std::string::find](http://www.cplusplus.com/reference/string/string/find/)
- [stackoverflow.com - How do I replace a string in C++?](https://stackoverflow.com/questions/4643512/replace-substring-with-another-substring-c)