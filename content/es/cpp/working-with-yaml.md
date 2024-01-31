---
title:                "Trabajando con YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabajando con YAML"
simple_title:         "Trabajando con YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Qué y por qué?
YAML es un formato de serialización de datos legible por humanos, usado para configuración, almacenamiento de datos y comunicación entre servicios. Los programadores lo utilizan por su simplicidad y la facilidad para mapear estructuras de lenguajes de programación como listas y diccionarios.

## Cómo hacerlo:
Para trabajar con YAML en C++, necesitas una biblioteca como yaml-cpp. Instala con `vcpkg install yaml-cpp` o similar. Ahora, veamos cómo cargar y escribir YAML.

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>
#include <fstream>
#include <string>

int main() {
    // Cargar YAML
    YAML::Node config = YAML::LoadFile("config.yaml");

    std::string username = config["user"]["name"].as<std::string>();
    int age = config["user"]["age"].as<int>();

    std::cout << "Name: " << username << ", Age: " << age << std::endl;

    // Escribir YAML
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "language" << YAML::Value << "C++";
    out << YAML::Key << "version" << YAML::Value << "11";
    out << YAML::EndMap;

    std::ofstream fout("output.yaml");
    fout << out.c_str();
    
    return 0;
}
```
Supone que `config.yaml` es:
```yaml
user:
  name: Juan
  age: 30
```
El resultado sería:
```
Name: Juan, Age: 30
```
Y genera `output.yaml`:
```yaml
language: C++
version: 11
```

## Deep Dive:
YAML, que significa "YAML Ain't Markup Language" (un acrónimo recursivo), apareció a principios de los años 2000 como una alternativa fácil de usar a XML. Otras opciones para serialización de datos incluyen JSON y Protobuf, cada uno con sus pros y contras. Mientras JSON es igual de legible, YAML es más limpio para la configuración por la ausencia de corchetes. Protobuf es más eficiente para la comunicación entre servicios, pero no es legible por humanos. En C++, yaml-cpp es la biblioteca más reconocida para manejar YAML y generalmente se prefiere por su API intuitiva y buen rendimiento.

## See Also:
- Documentación oficial de yaml-cpp: https://github.com/jbeder/yaml-cpp/wiki
- Especificación de YAML para entender el formato completo: https://yaml.org/spec/1.2/spec.html
- Tutorial interactivo de YAML para probar ejemplos en línea: https://learnxinyminutes.com/docs/yaml/
