---
title:                "Python: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por qué trabajar con YAML en Python

Python es uno de los lenguajes de programación más populares en la actualidad, utilizado en una amplia variedad de proyectos y aplicaciones. Una de las razones por las que Python es tan versátil es su capacidad para trabajar con diferentes formatos de datos, incluyendo YAML. En este post, exploraremos por qué trabajar con YAML puede ser beneficioso en tus proyectos de Python.

## Cómo utilizar YAML en Python

YAML (YAML Ain’t Markup Language) es un formato de serialización de datos legible por humanos. Con Python, podemos utilizar la librería `pyyaml` para trabajar con archivos YAML. Veamos un ejemplo de cómo podemos cargar y leer un archivo YAML en Python:

```
import yaml

# Cargamos el archivo YAML
with open('ejemplo.yaml') as file:
    datos = yaml.safe_load(file)

# Imprimimos los datos del archivo en consola
print(datos)
```

Suponiendo que el archivo `ejemplo.yaml` contiene los siguientes datos:

```
nombre: Juan Perez
edad: 30
lenguajes:
    - Python
    - JavaScript
```

La salida del código anterior sería:

```
{'nombre': 'Juan Perez', 'edad': 30, 'lenguajes': ['Python', 'JavaScript']}
```

Podemos ver que los datos del archivo YAML se han cargado correctamente en una estructura de diccionario de Python.

También podemos utilizar la librería `pyyaml` para escribir datos en un archivo YAML. Veamos un ejemplo:

```
import yaml

# Datos a escribir en el archivo YAML
datos = {
    'nombre': 'Maria Lopez',
    'edad': 25,
    'lenguajes': ['Java', 'C++']
}

# Escribimos los datos en el archivo YAML
with open('ejemplo2.yaml', 'w') as file:
    yaml.dump(datos, file)
```

Esto creará un archivo `ejemplo2.yaml` con los mismos datos que definimos en la estructura de datos de Python.

## Profundizando en YAML en Python

YAML es una excelente opción para manejar datos en formato legible por humanos. Es un formato fácil de aprender y entender, lo que lo hace ideal para trabajar con archivos de configuración o intercambio de datos. Con Python y la librería `pyyaml`, podemos trabajar con archivos YAML de manera muy sencilla y eficiente.

Otra ventaja de YAML es su capacidad para representar estructuras de datos más complejas, como listas o diccionarios anidados. También cuenta con una sintaxis flexible que nos permite definir comentarios y referencias a otras partes del archivo.

En general, trabajar con YAML en Python puede simplificar en gran medida la manipulación y lectura de datos. Además, su formato legible por humanos lo hace muy útil para colaborar con otros desarrolladores y compartir datos de forma clara y ordenada.

## Ver también

- [Documentación de pyyaml](https://pyyaml.org/wiki/PyYAMLDocumentation)
- [Tutorial de YAML en Python](https://www.datacamp.com/community/tutorials/reading-writing-files-python-pyyaml)
- [Ejemplos de utilización de YAML en proyectos de Python](https://www.programcreek.com/python/example/22533/yaml)