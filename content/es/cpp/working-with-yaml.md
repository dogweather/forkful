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

## Qué y Por Qué?

Trabajar con YAML es una forma de estructurar los datos en el código de programación de forma legible y fácilmente manejable. Se utiliza para crear archivos de configuración y datos en un formato basado en texto plano. Los programadores utilizan YAML para organizar y almacenar información de una manera clara y ordenada, lo que hace que sea más sencillo trabajar con ella en sus programas.

## Cómo:

Los ejemplos de código y su correspondiente resultado están incluidos en bloques de código ```C++ ... ```.

#### Ejemplo 1: Crear un archivo YAML
```C++
#include <iostream> 
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "nombre";
    out << YAML::Value << "Juan";
    out << YAML::Key << "edad";
    out << YAML::Value << 25;
    out << YAML::EndMap;
    
    std::cout << out.c_str() << "\n";
    
    return 0;
}
```
#### Resultado:
```
nombre: Juan
edad: 25 
```

#### Ejemplo 2: Leer un archivo YAML
```C++
#include <iostream> 
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Node datos = YAML::LoadFile("datos.yaml");
    std::cout << datos["nombre"].as<std::string>() << "\n";
    std::cout << datos["edad"].as<int>() << "\n";
    
    return 0;
}
```
#### Datos.yaml:
```
nombre: Juan
edad: 25
```
#### Resultado:
```
Juan
25
```

## Deep Dive:

YAML significa "YAML Ain't Markup Language" y es un formato de serialización de datos que se basa en texto plano. Fue creado en 2001 por Ingy döt Net y es utilizado principalmente para crear archivos de configuración en lenguajes de programación. Algunas alternativas a YAML incluyen JSON y XML, pero YAML es popular debido a su sintaxis limpia y fácil legibilidad para los humanos. La implementación de YAML se realiza utilizando librerías y paquetes como "yaml-cpp".

## Ver también:

- [Sitio web oficial de YAML](https://yaml.org/)
- [YAML vs. JSON: ¿cuál usar?](https://www.baeldung.com/yaml-vs-json)
- [Documentación de yaml-cpp](https://github.com/jbeder/yaml-cpp/wiki)