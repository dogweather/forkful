---
title:                "Concatenando cadenas de texto"
html_title:           "Arduino: Concatenando cadenas de texto"
simple_title:         "Concatenando cadenas de texto"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Concatenar es el proceso de unir dos o más cadenas de caracteres en una sola. Los programadores concatenan cadenas para manipular datos o generar nuevas cadenas a partir de existentes.

## Cómo hacerlo:

Aquí te proporciono un ejemplo sencillo de cómo concatenar cadenas en C++:

```C++
#include <string>
#include <iostream>

int main() {
    std::string cadena1 = "Hola, ";
    std::string cadena2 = "mundo!";
    std::string cadena_unida = cadena1 + cadena2;
    std::cout << cadena_unida;

    return 0;
}
```

La salida de este programa será: 
```C++
Hola, mundo!
```

## Análisis más profundo

1. **Contexto histórico:** La concatenación de cadenas se considera una operación fundamental en la programación desde los primeros días de los lenguajes de programación.

2. **Alternativas:** En C++, hay múltiples formas de concatenar cadenas. Puedes usar el operador `+`, como en el ejemplo anterior, o el método `append()` proporcionado por la clase `std::string`:
    ```C++
    std::string cadena1 = "Hola, ";
    std::string cadena2 = "mundo!";
    cadena1.append(cadena2); // cadena1 ahora contiene "Hola, mundo!"
    ```
    Ambas opciones tienen sus propios usos dependiendo de la situación.
  
3. **Implementación:** La concatenación de cadenas es una operación costosa en términos de rendimiento. Cada concatenación puede resultar en la creación de una nueva cadena y la copia de los caracteres antiguos a la nueva cadena. Lo anterior es especialmente notable cuando se concatenan grandes cantidades de strings.

## Consulta también

Para más información sobre las operaciones con cadenas en C++, puedes consultar las siguientes fuentes:

- Documentación oficial de `std::string` en [cppreference](http://cppreference.com/).
  
- Para entender cómo las operaciones de concatenación pueden afectar el rendimiento, consulta [este artículo](https://www.cplusplus.com/articles/ET3j1bDs/).

- Para comprender mejor cómo funciona el operador `+`, consulta [este enlace](https://en.cppreference.com/w/cpp/string/basic_string/operator%2B).