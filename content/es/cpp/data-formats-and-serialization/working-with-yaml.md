---
title:                "Trabajando con YAML"
date:                  2024-02-03T19:24:31.051461-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

YAML, que significa "YAML Ain't Markup Language" (YAML no es un lenguaje de marcado), es un formato de serialización de datos legible por humanos. Los programadores lo utilizan para archivos de configuración, volcado de datos y almacenamiento de datos jerárquicos debido a su legibilidad y sintaxis fácil de entender en comparación con XML o JSON.

## Cómo hacerlo:

Para trabajar con YAML en C++, una opción popular es la biblioteca `yaml-cpp`. Primero, asegúrate de tener `yaml-cpp` instalado y correctamente vinculado a tu proyecto de C++.

**Leyendo un archivo YAML:**

```cpp
#include <iostream>
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Node config = YAML::LoadFile("config.yaml");
    
    if(config["title"]) {
        std::cout << "Título: " << config["title"].as<std::string>() << std::endl;
    }
    
    return 0;
}
```

Dado un `config.yaml` que luce así:

```yaml
title: "Ejemplo YAML"
```

Ejecutar el código C++ anterior produciría:

```
Título: Ejemplo YAML
```

**Escribiendo en un archivo YAML:**

```cpp
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "title" << YAML::Value << "Ejemplo YAML";
    out << YAML::EndMap;
    
    std::ofstream fout("output.yaml");
    fout << out.c_str();
    
    return 0;
}
```

Este código creará un `output.yaml` con el contenido:

```yaml
title: Ejemplo YAML
```

Estos ejemplos sirven como una introducción básica a la lectura y escritura de archivos YAML en C++ utilizando la biblioteca `yaml-cpp`. Para estructuras más complejas y casos de uso, explora la documentación de `yaml-cpp` para características como secuencias, etiquetas y técnicas de serialización y deserialización más avanzadas.
