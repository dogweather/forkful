---
title:                "Trabajando con YAML"
html_title:           "Arduino: Trabajando con YAML"
simple_title:         "Trabajando con YAML"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Trabajar con YAML implica manipular un formato de datos legible por humanos, común en configuraciones y archivos de despliegue. Los programadores lo hacen por su simplicidad y facilidad de uso en diversas aplicaciones, especialmente en DevOps.

## Cómo hacerlo:

Instalación de las herramientas de YAML en Bash:
```Bash
sudo apt-get update
sudo apt-get install -y python3-pip
pip3 install pyyaml
```

Leer un archivo YAML:
```Bash
python3 -c 'import yaml; print(yaml.safe_load(open("archivo.yaml")))'
```

Escribir en un archivo YAML:
```Bash
python3 -c 'import yaml; data = {"clave": "valor"}; open("salida.yaml", "w").write(yaml.dump(data))'
```

## Profundización

YAML, que significa "YAML Ain't Markup Language" (YAML no es un lenguaje de marcado), surgió en 2001 como una alternativa a XML para datos más fáciles de leer y escribir. Aunque JSON es otro formato popular por su simplicidad, YAML es más adecuado para configuraciones humanos debido a su alta legibilidad. En la implementación, herramientas como PyYAML en Python permiten manipular YAML de manera eficiente en scripts de Bash.

## Ver También

- Documentación oficial de YAML: https://yaml.org/spec/1.2/spec.html
- PyYAML en GitHub: https://github.com/yaml/pyyaml
- Tutorial de YAML: https://learnxinyminutes.com/docs/yaml/