---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:33.361324-07:00
description: "YAML, que significa \"YAML Ain't Markup Language\" (YAML no es un lenguaje\
  \ de marcado), es un formato de serializaci\xF3n de datos legible por humanos. Los\u2026"
lastmod: '2024-03-11T00:14:32.464056-06:00'
model: gpt-4-0125-preview
summary: "YAML, que significa \"YAML Ain't Markup Language\" (YAML no es un lenguaje\
  \ de marcado), es un formato de serializaci\xF3n de datos legible por humanos. Los\u2026"
title: Trabajando con YAML
---

{{< edit_this_page >}}

## ¿Qué y por qué?
YAML, que significa "YAML Ain't Markup Language" (YAML no es un lenguaje de marcado), es un formato de serialización de datos legible por humanos. Los programadores utilizan YAML para archivos de configuración, mensajería entre procesos y almacenamiento de datos debido a su sintaxis simple y facilidad de lectura en comparación con otros formatos como XML o JSON.

## Cómo hacerlo:
Leer y escribir YAML en Python generalmente implica el uso de una biblioteca de terceros, siendo `PyYAML` la más popular. Para comenzar, necesitarás instalar PyYAML ejecutando `pip install PyYAML`.

**Ejemplo: Escribir en un archivo YAML**

```python
import yaml

data = {'una lista': [1, 42, 3.141, 1337, 'ayuda', u'€'],
        'una cadena': '¡bu!',
        'otro diccionario': {'foo': 'bar', 'clave': 'valor', 'la respuesta': 42}}

with open('ejemplo.yaml', 'w') as f:
    yaml.dump(data, f, default_flow_style=False)

# Esto crea `ejemplo.yaml` con los datos estructurados en formato YAML.
```

**Ejemplo: Leer de un archivo YAML**

```python
import yaml

with open('ejemplo.yaml', 'r') as f:
    data_cargada = yaml.safe_load(f)

print(data_cargada)

# Salida:
# {'una lista': [1, 42, 3.141, 1337, 'ayuda', '€'],
#  'una cadena': '¡bu!',
#  'otro diccionario': {'foo': 'bar', 'clave': 'valor', 'la respuesta': 42}}
```

**Usar YAML para Configuración**

Muchos programadores usan YAML para gestionar configuraciones de aplicaciones. Aquí tienes un ejemplo de cómo uno podría estructurar un archivo de configuración y leerlo:

config.yaml:
```yaml
base_de_datos:
  host: localhost
  puerto: 5432
  nombre_usuario: admin
  contraseña: secreto
```

Leyendo el archivo de configuración en Python:
```python
import yaml

with open('config.yaml', 'r') as f:
    config = yaml.safe_load(f)

print(config['base_de_datos']['host'])  # Salida: localhost
```

**Manejo de Estructuras Complejas**

Para estructuras complejas, PyYAML te permite definir objetos Python personalizados. Sin embargo, asegúrate de practicar métodos seguros utilizando `safe_load` para evitar ejecutar funciones u objetos arbitrarios.

```python
import yaml

# Definir un objeto Python
class Ejemplo:
    def __init__(self, valor):
        self.valor = valor

# Constructor personalizado
def constructor_ejemplo(loader, node):
    valor = loader.construct_scalar(node)
    return Ejemplo(valor)

# Añadir constructor para la etiqueta "!example"
yaml.add_constructor('!example', constructor_ejemplo)

yaml_str = "!example 'datos'"
cargado = yaml.load(yaml_str, Loader=yaml.FullLoader)

print(cargado.valor)  # Salida: datos
```

En este fragmento, `!example` es una etiqueta personalizada utilizada para instanciar un objeto `Ejemplo` con el valor 'datos' de una cadena YAML. Cargadores personalizados como este aumentan la flexibilidad de PyYAML, permitiendo el procesamiento de estructuras de datos y tipos más complejos.
