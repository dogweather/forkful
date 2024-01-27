---
title:                "Trabajando con YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabajando con YAML"
simple_title:         "Trabajando con YAML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
YAML es un formato para guardar objetos de datos con estructura legible por humanos. Programadores lo usan por su simplicidad y facilidad de integración con distintos lenguajes y herramientas.

## Cómo Hacerlo:
Arduino no tiene bibliotecas estandarizadas para trabajar directamente con YAML, pero puedes integrar YAML-CPP u otras bibliotecas C++ compatibles si necesitas trabajar con este formato. Aquí tienes un ejemplo básico en C++ que luego podrías adaptar para Arduino:

```cpp
#include <yaml-cpp/yaml.h>
#include <iostream>
#include <string>

int main() {
    YAML::Node config = YAML::Load("name: Arduino\nversion: 1.0\n");

    std::string name = config["name"].as<std::string>();
    float version = config["version"].as<float>();

    std::cout << "Name: " << name << "\nVersion: " << version << std::endl;

    return 0;
}
```

Output esperado:
```
Name: Arduino
Version: 1.0
```

## Mayor Profundidad:
YAML, que significa "YAML Ain't Markup Language" (un acrónimo recursivo), surgió en 2001 como una alternativa más legible a XML o JSON. Aunque YAML es menos común en Arduino debido a la limitación de recursos de los microcontroladores, puede ser usado en aplicaciones IoT donde la configuración o datos se integran con sistemas más complejos. JSON es una alternativa frecuente por su ligereza y compatibilidad nativa con muchos sistemas.

## Ver También:
- Documentación oficial de YAML: https://yaml.org/
- YAML-CPP, una biblioteca para procesar YAML en C++: https://github.com/jbeder/yaml-cpp
- JSON en Arduino con la biblioteca ArduinoJson: https://arduinojson.org/
