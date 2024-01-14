---
title:                "C++: Concatenando cadenas"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

La concatenación de cadenas de texto es una técnica fundamental en la programación en C++. Al combinar varias cadenas en una sola, podemos crear mensajes más complejos y dinámicos, lo que es esencial para crear programas verdaderamente útiles. ¡Sigue leyendo para aprender cómo hacerlo!

## Cómo hacerlo

En C++, podemos concatenar cadenas de texto usando el operador "+" o la función "append". Veamos algunos ejemplos:

```C++
// Ejemplo con operador "+"
string nombre = "Juan";
string apellido = "Pérez";
string nombre_completo = nombre + " " + apellido;
cout << nombre_completo << endl;
// Output: Juan Pérez

// Ejemplo con función "append"
string mensaje = "Hola";
string exclamacion = "!";
mensaje.append(exclamacion);
cout << mensaje << endl;
// Output: Hola!
```

Como puedes ver, tanto el operador "+" como la función "append" nos permiten combinar diferentes cadenas de texto en una sola. Además, podemos incorporar otros elementos, como espacios o signos de puntuación, para crear mensajes más elaborados.

## Inmersión profunda

Detrás de la concatenación de cadenas de texto en C++ se encuentra el concepto de arreglos de caracteres o "char arrays". En esencia, cada cadena de texto es un arreglo de caracteres, y al concatenarlas simplemente estamos combinando estos arreglos. Por lo tanto, es importante tener en cuenta que, al modificar una cadena de texto, también podemos estar modificando su arreglo de caracteres subyacente. Esto puede ser especialmente útil al trabajar con cadenas de caracteres en un contexto más amplio.

## Ver también

- [Documentación oficial de C++ sobre cadenas de texto](https://en.cppreference.com/w/cpp/string/basic_string)
- [Cómo imprimir variables en C++](https://platzi.com/blog/impresion-de-variables-en-cpp/)
- [Cómo crear estructuras de control en C++](https://platzi.com/blog/estructuras-de-control-cpp/)