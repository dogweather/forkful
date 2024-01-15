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

# ¿Por qué trabajar con YAML en Python?

YAML, que significa "YAML Ain't Markup Language" es un lenguaje de serialización de datos basado en texto que se utiliza comúnmente para almacenar y transferir datos estructurados. En el campo de la programación, YAML se utiliza a menudo para la configuración de aplicaciones y la transferencia de datos entre componentes de una aplicación. Trabajar con YAML en Python puede simplificar el proceso de almacenamiento y transferencia de datos, lo que hace que sea una herramienta valiosa para cualquier programador.

## Cómo trabajar con YAML en Python

Para comenzar a trabajar con YAML en Python, primero debes importar el módulo "yaml" en tu código. Puedes hacerlo de la siguiente manera:

```python
import yaml
```

A continuación, puedes utilizar la función `load()` del módulo para cargar los datos YAML que quieras utilizar en tu programa. Por ejemplo, si tienes un archivo "datos.yaml" con la siguiente información:

```yaml
nombre: Juan
edad: 25
trabajo: Desarrollador
```

Puedes cargar estos datos en un diccionario de Python de la siguiente manera:

```python
with open('datos.yaml') as archivo:
    datos = yaml.load(archivo, Loader=yaml.FullLoader)

print(datos)
```

El resultado sería un diccionario de Python con los datos de tu archivo YAML:

```python
{'nombre': 'Juan', 'edad': 25, 'trabajo': 'Desarrollador'}
```

También puedes utilizar la función `dump()` para almacenar datos dictamen en un archivo YAML. Por ejemplo, si quieres almacenar el mismo diccionario en un nuevo archivo "salida.yaml", puedes hacerlo de la siguiente manera:

```python
nuevo_dato = {'ciudad': 'Madrid', 'país': 'España'}

with open('salida.yaml', 'w') as archivo:
    yaml.dump(nuevo_dato, archivo)
```

Esto creará un nuevo archivo "salida.yaml" con los datos del diccionario almacenados en formato YAML:

```yaml
ciudad: Madrid
país: España
```

## Profundizando en YAML en Python

Además de cargar y descargar datos YAML, también puedes trabajar con ellos directamente en tu código utilizando la sintaxis YAML. Por ejemplo, si necesitas crear una lista en tu programa, puedes hacerlo utilizando la sintaxis YAML:

```yaml
- manzanas
- peras
- plátanos
```

Este código YAML crearía una lista con los elementos "manzanas", "peras" y "plátanos". Puedes asignar esta lista a una variable en tu código y utilizarla como cualquier otra lista de Python.

Además, YAML también te permite utilizar comentarios para documentar tu código. Puedes añadir comentarios utilizando el símbolo de almohadilla "#" al principio de una línea:

```yaml
# Este es un comentario
nombre: María # Esto es también un comentario
```

Al igual que con la sintaxis YAML, puedes utilizar comentarios en tu código YAML directamente, lo que hace que sea una herramienta útil para la documentación de tu código.

# Ver también

- [Documentación oficial de YAML] (https://yaml.org/)
- [Tutorial de YAML en Python] (https://realpython.com/python-yaml/)
- [Especificación de YAML] (https://yaml.org/spec/)