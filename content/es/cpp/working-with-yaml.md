---
title:                "Trabajando con yaml"
html_title:           "C++: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por qué trabajar con YAML

Si eres un programador en busca de una forma sencilla de estructurar datos y configuraciones en tus proyectos, YAML puede ser la solución perfecta para ti. Esta sintaxis ligera y fácil de leer es cada vez más popular en el mundo de la programación gracias a su flexibilidad y soporte en una amplia variedad de lenguajes.

## Cómo trabajar con YAML

Para trabajar con YAML en C++, primero necesitamos incluir la librería correspondiente en nuestro código:

```C++
#include <yaml-cpp/yaml.h>
```

A continuación, podemos crear un objeto de tipo `YAML::Node` que contendrá nuestros datos estructurados en formato YAML:

```C++
YAML::Node datos;
```

Podemos agregar datos a nuestro objeto utilizando la sintaxis de YAML, que consiste en pares clave-valor separados por dos puntos y líneas nuevas:

```C++
datos["nombre"] = "Ana";
datos["edad"] = 25;
datos["ciudad"] = "Madrid";
```

Para acceder a los datos almacenados en nuestro objeto, podemos utilizar el operador de acceso con corchetes y la clave correspondiente:

```C++
cout << "La edad de " << datos["nombre"] << " es " << datos["edad"] << " años.";
```

Ahora, si imprimimos el objeto, veremos el equivalente YAML de nuestros datos:

```C++
nombre: Ana
edad: 25
ciudad: Madrid
```

¡Así de fácil es trabajar con YAML en C++!

## Profundizando en YAML

Además de guardar datos simples como en el ejemplo anterior, YAML también permite estructurar datos más complejos, como listas y objetos, de manera muy intuitiva. Podemos incluso incluir comentarios en nuestro código YAML utilizando el carácter numeral `#`.

Otra ventaja de trabajar con YAML en C++ es que la librería `yaml-cpp` nos ofrece una gran variedad de funciones y métodos para manipular y acceder a nuestros datos de forma eficiente.

Si quieres aprender más sobre YAML y sus posibilidades, te recomendamos revisar la documentación oficial de `yaml-cpp` y practicar con diferentes ejemplos.

## Ver también

- [Documentación oficial de de yaml-cpp](https://yaml-cpp.github.io/)
- [Tutorial de YAML en C++](https://www.learncpp.com/cpp-tutorial/08-yaml/)
- [Ejemplos de uso de YAML en proyectos reales](https://github.com/jbeder/yaml-cpp/wiki/Tutorial)