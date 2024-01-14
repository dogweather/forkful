---
title:                "C++: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

# Por qué trabajar con YAML en C++

Si eres un programador de C++ y estás buscando una forma más eficiente y estructurada de almacenar y compartir datos en tus programas, entonces trabajar con YAML puede ser una gran opción para ti.

## Cómo hacerlo

En primer lugar, es importante saber que YAML es un formato de serialización de datos que se utiliza para representar información de manera legible tanto para humanos como para máquinas. Esto lo hace especialmente útil para la comunicación entre diferentes programas y sistemas.

Para empezar a trabajar con YAML en C++, es necesario incluir la librería YAML en tu código. En la siguiente sección de código se muestra cómo hacerlo:

```C++
#include <yaml-cpp/yaml.h>
```

Una vez que tengas la librería instalada y añadida en tu código, puedes comenzar a crear y leer archivos YAML en C++ utilizando la clase `YAML::Node`. A continuación, se muestra un ejemplo de cómo crear un archivo YAML con algunos datos y luego leerlo:

```C++
YAML::Node datos;
datos["nombre"] = "Ana";
datos["edad"] = 25;
datos["hobbies"] = ["cantar", "bailar", "programar"];

// Escribir el archivo YAML
std::ofstream archivo("datos.yaml");
archivo << datos;
archivo.close();

// Leer el archivo YAML
YAML::Node nuevo_dato = YAML::LoadFile("datos.yaml");
```

Output:

```
nombre: Ana
edad: 25
hobbies:
- cantar
- bailar
- programar
```

Puedes ver que los datos se almacenan en pares clave-valor, y que incluso puedes almacenar listas como valores. También puedes cargar directamente un archivo YAML existente utilizando el método `YAML::LoadFile()`.

## Profundizando

Ahora que ya sabes cómo crear y leer archivos YAML en C++, aquí hay algunas cosas más que puedes hacer con esta librería:

- Validar y verificar archivos YAML para asegurarte de que cumplen con la estructura adecuada.
- Acceder y modificar datos específicos en un archivo YAML utilizando los métodos `YAML::Node::operator[]` y `YAML::Node::remove()`.
- Incluso puedes fusionar varios archivos YAML en uno solo utilizando el método `YAML::operator+()`.

Con todas estas funcionalidades, puedes controlar y manipular datos de manera eficiente y conveniente en tus programas de C++, sin tener que preocuparte por la estructura y formato de los mismos.

# Ver también

- [Documentación oficial de la librería YAML-CPP](https://github.com/jbeder/yaml-cpp/wiki)
- [Tutorial sobre cómo trabajar con YAML en C++](https://www.geeksforgeeks.org/yaml-in-cpp/)
- [Ejemplos de uso de la librería YAML-CPP en proyectos reales](https://github.com/jbeder/yaml-cpp/tree/master/examples)