---
title:                "Creando un archivo temporal"
html_title:           "Python: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Qué & Por qué?
Crear un archivo temporal es una práctica común en la programación que permite a los desarrolladores almacenar información temporalmente durante la ejecución del código. Esto es especialmente útil para guardar datos que no necesitan ser permanentes, como archivos de configuración o registros de eventos.

## Cómo:
Para crear un archivo temporal en Python, puedes utilizar la función ```mkstemp()``` del módulo ```tempfile```. Esta función crea un archivo con un nombre y extensión aleatorios en la ubicación especificada. También puedes usar la función ```NamedTemporaryFile()```, que crea automáticamente un archivo temporal en el sistema de archivos y devuelve un objeto de archivo que puedes utilizar para escribir y leer contenido en él.

Ejemplo de uso de la función ```mkstemp()```:

```
import tempfile
import os

archivo_temporal, ruta = tempfile.mkstemp()

print("Ruta del archivo temporal:", ruta) 
print("Nombre del archivo temporal:", os.path.basename(ruta))
```

Salida:
```
Ruta del archivo temporal: /tmp/tmp03ls5pl0
Nombre del archivo temporal: tmp03ls5pl0
```

Ejemplo de uso de la función ```NamedTemporaryFile()```:

```
import tempfile

with tempfile.NamedTemporaryFile() as archivo_temporal:
    print("Ruta del archivo temporal:", archivo_temporal.name)
    archivo_temporal.write(b"Ejemplo de contenido temporal")
    archivo_temporal.seek(0)
    print("Contenido del archivo temporal:", archivo_temporal.read())
```

Salida:
```
Ruta del archivo temporal: /tmp/tmp97fjz1pm
Contenido del archivo temporal: b'Ejemplo de contenido temporal'
```

## Profundizando:
Aunque crear archivos temporales es una práctica común y útil, es importante tener en cuenta que el uso de memoria temporal también puede ser un tema de seguridad, ya que estos archivos pueden almacenar información confidencial. Por lo tanto, siempre es recomendable borrar los archivos temporales una vez que ya no se necesitan mediante la función ```os.remove()```.

Como alternativa a las funciones mencionadas anteriormente, también puedes utilizar el módulo ```tempfile.TemporaryFile()```, que crea un archivo temporal de manera similar a ```NamedTemporaryFile()``` pero lo borra automáticamente una vez que se cierra.

Para aquellos que quieren implementar su propia lógica de limpieza del archivo temporal, también existe la opción de utilizar la función ```mkdtemp()``` del módulo ```tempfile``` que crea un directorio temporal en lugar de un archivo.

## Ver también:
Puedes obtener más información sobre la creación de archivos temporales en Python en la documentación oficial del lenguaje en el módulo ```tempfile``` aquí: https://docs.python.org/3/library/tempfile.html