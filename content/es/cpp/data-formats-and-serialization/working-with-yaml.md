---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:31.051461-07:00
description: "C\xF3mo hacerlo: Para trabajar con YAML en C++, una opci\xF3n popular\
  \ es la biblioteca `yaml-cpp`. Primero, aseg\xFArate de tener `yaml-cpp` instalado\
  \ y\u2026"
lastmod: '2024-03-13T22:44:59.395233-06:00'
model: gpt-4-0125-preview
summary: "Para trabajar con YAML en C++, una opci\xF3n popular es la biblioteca `yaml-cpp`."
title: Trabajando con YAML
weight: 41
---

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
