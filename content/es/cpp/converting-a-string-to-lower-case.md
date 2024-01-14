---
title:                "C++: Convirtiendo una cadena a minúsculas"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una cadena a minúsculas?

Convertir una cadena de texto a minúsculas puede ser útil en varias situaciones, como por ejemplo cuando se necesita comparar dos cadenas sin tener en cuenta las mayúsculas y minúsculas, o para mejorar la presentación visual de una palabra o frase en un programa.

## Cómo hacerlo

Se puede realizar la conversión de una cadena a minúsculas de varias maneras en C++. Una opción es utilizar la función `tolower()` de la biblioteca estándar. Por ejemplo:

```C++
std::string texto = "PROGRAMACIÓN EN C++";
std::transform(texto.begin(), texto.end(), texto.begin(), ::tolower);
std::cout << texto << std::endl;
```

El resultado de este código sería "programación en c++". Otro método es utilizar un bucle for para recorrer cada uno de los caracteres de la cadena y reemplazar las letras mayúsculas con sus equivalentes en minúsculas utilizando la tabla ASCII.

```C++
std::string texto = "PROGRAMACIÓN EN C++";
for (int i = 0; i < texto.length(); i++) {
    if (texto[i] >= 65 && texto[i] <= 90) {
        texto[i] = texto[i] + 32; // La diferencia entre mayús. y minús. en ASCII es de 32
    }
}
std::cout << texto << std::endl;
```

Este código también produciría como resultado "programación en c++". Ambos métodos son válidos y tienen sus propias ventajas y desventajas, por lo que se puede elegir el que mejor se adapte a la situación y preferencia del programador.

## Profundizando en la conversión de cadenas a minúsculas

Es importante tener en cuenta que no todas las letras tienen equivalentes en mayúsculas y minúsculas en todos los idiomas, por lo que es necesario tener cuidado con la codificación y el contexto en el que se necesita realizar esta conversión. Además, existen otras formas de realizar la conversión de una cadena a minúsculas, como utilizando expresiones regulares o bibliotecas externas específicas para manejar texto en diferentes idiomas.

## Ver también

- [10 Formas de Manipular Cadenas en C++](https://www.educative.io/edpresso/10-formas-de-manipular-cadenas-en-cpp)
- [La importancia de la codificación de caracteres en programación](https://dev.to/njorogeth/la-importancia-de-la-codificaci-n-de-caracteres-en-programaci-n-2g7p)