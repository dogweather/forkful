---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:04.323999-07:00
description: "C\xF3mo hacerlo: En C++, puedes capitalizar un string utilizando la\
  \ biblioteca est\xE1ndar sin necesidad de bibliotecas de terceros. Sin embargo,\
  \ para\u2026"
lastmod: '2024-03-13T22:44:59.358917-06:00'
model: gpt-4-0125-preview
summary: "En C++, puedes capitalizar un string utilizando la biblioteca est\xE1ndar\
  \ sin necesidad de bibliotecas de terceros."
title: Capitalizando una cadena de texto
weight: 2
---

## Cómo hacerlo:
En C++, puedes capitalizar un string utilizando la biblioteca estándar sin necesidad de bibliotecas de terceros. Sin embargo, para comportamientos de capitalización más complejos o específicos, bibliotecas como Boost pueden ser bastante útiles. A continuación, se muestran ejemplos que ilustran ambos enfoques.

### Usando la Biblioteca Estándar de C++:
```cpp
#include <iostream>
#include <cctype> // para std::tolower y std::toupper
#include <string>

std::string capitalizeString(const std::string& entrada) {
    std::string resultado;
    bool capitalizarSiguiente = true;

    for (char ch : entrada) {
        if (std::isspace(ch)) {
            capitalizarSiguiente = true;
        } else if (capitalizarSiguiente) {
            ch = std::toupper(ch);
            capitalizarSiguiente = false;
        }
        resultado += ch;
    }

    return resultado;
}

int main() {
    std::string texto = "hello world from c++";
    std::string textoCapitalizado = capitalizeString(texto);
    std::cout << textoCapitalizado << std::endl; // Salida: "Hello World From C++"
}
```

### Usando la Biblioteca Boost:
Para manipulaciones de strings más avanzadas, incluyendo la capitalización consciente de la configuración regional, podrías querer usar la biblioteca Boost String Algo.

Primero, asegúrate de tener la biblioteca Boost instalada y configurada en tu proyecto. Luego puedes incluir las cabeceras necesarias y usar sus características como se muestra a continuación.

```cpp
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <string>

int main() {
    std::string texto = "hello world from c++";
    std::string textoCapitalizado = texto;

    // capitalizar la primera letra de cada palabra
    boost::algorithm::to_lower(textoCapitalizado); // asegurando que el string esté en minúscula
    textoCapitalizado[0] = std::toupper(textoCapitalizado[0]); // capitalizar el primer carácter

    for (std::size_t i = 1; i < textoCapitalizado.length(); ++i) {
        if (isspace(textoCapitalizado[i - 1])) { // capitalizar después de un espacio
            textoCapitalizado[i] = std::toupper(textoCapitalizado[i]);
        }
    }

    std::cout << textoCapitalizado << std::endl; // Salida: "Hello World From C++"
}
```

En este caso, Boost simplifica algunas de las tareas de manipulación de strings pero aún requiere un enfoque personalizado para una verdadera capitalización, ya que principalmente ofrece utilidades de transformación y conversión de casos.
