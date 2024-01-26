---
title:                "Trabajando con YAML"
html_title:           "Arduino: Trabajando con YAML"
simple_title:         "Trabajando con YAML"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Qué es y Por Qué?

YAML es un formato para guardar objetos de datos que es legible por humanos. Los programadores lo usan porque es más simple y claro que formatos como XML o JSON para ciertos tipos de datos y configuraciones.

## Cómo:

Para trabajar con YAML en Python, deberás usar `PyYAML`, una biblioteca que facilita la carga y escritura de datos YAML. Aquí tienes unos ejemplos básicos:

### Instalar PyYAML

```Python
pip install PyYAML
```

### Leer YAML

```Python
import yaml

# Lee un archivo YAML
with open('datos.yaml', 'r') as file:
    datos = yaml.safe_load(file)

print(datos)
```

### Escribir YAML

```Python
import yaml

datos = {'clave': 'valor', 'lista': [1, 2, 3]}

# Escribe datos a un archivo YAML
with open('datos.yaml', 'w') as file:
    yaml.dump(datos, file)
```

## Inmersión Profunda

YAML, que significa "YAML Ain't Markup Language" (YAML no es un lenguaje de marcado), se desarrolló para ser accesible y cómodo para guardar datos de configuración y serialización. Alternativas populares incluyen JSON, considerado más compacto para la web, y XML, que es más detallado y extenso. Un detalle clave de implementación de YAML es su capacidad para representar relaciones de datos complejas, como referencias cíclicas.

## Ver También

- Documentación oficial de PyYAML: https://pyyaml.org/
- Especificación YAML 1.2: https://yaml.org/spec/1.2/spec.html
- Tutorial YAML: https://www.tutorialspoint.com/yaml/index.htm
