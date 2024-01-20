---
title:                "Trabajando con yaml"
html_title:           "Python: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Trabajar con YAML es una forma de manejar datos estructurados en formato de texto plano. Los programadores lo usan principalmente para configurar aplicaciones y compartir datos entre diferentes sistemas. También es útil para crear y gestionar archivos de configuración de manera sencilla y legible.

## Cómo:

Para empezar a trabajar con YAML en Python, primero debes importar el módulo PyYAML. Luego, puedes utilizar la función safe_load para cargar y leer un archivo YAML. Aquí hay un ejemplo:

```Python
import yaml

# Cargar datos desde un archivo YAML
with open('datos.yaml') as f:
    datos = yaml.safe_load(f)

# Acceder a los datos cargados
print(datos['nombre'])  # imprimirá "Python"
```

Además de cargar y leer datos YAML, también puedes utilizar la función dump para convertir objetos de Python en formato YAML. Aquí hay un ejemplo:

```Python
import yaml

# Crear un objeto de Python
datos = {
    'nombre': 'Python',
    'versión': 3.8,
}

# Convertir a formato YAML
print(yaml.dump(datos))  # imprimirá "nombre: Python\nversión: 3.8\n"
```

## Profundizando:

El formato YAML fue creado en 2001 por Clark Evans y está inspirado en otros lenguajes de marcado como XML y JSON. Aunque existen otras alternativas como JSON o TOML, YAML se ha vuelto muy popular en los últimos años debido a su estructura fácil de leer y escribir.

En cuanto a la implementación de YAML en Python, existen varias bibliotecas que pueden ser utilizadas, pero la más utilizada es PyYAML, que está disponible a través del comando "pip install PyYAML".

## Ver también:

- [Documentación oficial de PyYAML](https://pyyaml.org/wiki/PyYAMLDocumentation)
- [Introducción a YAML en Python](https://realpython.com/python-yaml/)