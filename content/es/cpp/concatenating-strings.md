---
title:                "Uniendo cadenas"
html_title:           "C++: Uniendo cadenas"
simple_title:         "Uniendo cadenas"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

¿Qué es y por qué los programadores la usan?

La concatenación de cadenas es un proceso en el que se unen dos o más cadenas de texto para formar una sola cadena. Los programadores lo hacen para combinar diferentes cadenas y crear un texto más largo y complejo.

## ¿Cómo hacerlo?

Para concatenar cadenas en C++, hay varias formas de hacerlo. Una forma común es usar el operador `+` para unir dos cadenas:

```C++
string s1 = "Hola";
string s2 = "mundo";
string s3 = s1 + s2; // s3 será "Hola mundo"
```

También se puede usar la función `concat` de la clase `string`:

```C++
string s1 = "Hola";
string s2 = "mundo";
string s3 = s1.concat(s2); // s3 será "Hola mundo"
```

También se puede usar la función `append` de la clase `string`, que agrega la cadena especificada al final de la cadena actual:

```C++
string s1 = "Hola";
string s2 = "mundo";
string s3 = s1.append(s2); // s3 será "Hola mundo"
```

## Inmersion Profunda

La concatenación de cadenas ha sido usada desde los primeros días de la programación de computadoras. Antes de los lenguajes de programación modernos, se usaban métodos más complicados, como la unión y la separación de cadenas de caracteres, para lograr el mismo resultado. En la actualidad, la concatenación de cadenas es una técnica común para construir mensajes y mensajes de error para los usuarios.

En C++, también existe la posibilidad de usar el tipo de dato `char*`, que representa un puntero a una cadena de caracteres. Al concatenar con este tipo de dato, debe tenerse cuidado para no sobrescribir o corromper la memoria.

## Ver también

- [La documentación oficial de C++ sobre la clase string](https://docs.microsoft.com/en-us/cpp/standard-library/string-class?view=msvc-160)
- [Una guía más detallada sobre la concatenación de cadenas en C++](https://www.studytonight.com/cpp/string-concatenation-in-cpp)