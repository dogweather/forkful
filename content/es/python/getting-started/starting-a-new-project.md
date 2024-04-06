---
changelog:
- 2024-02-22, dogweather, reviewed
- 2024-02-22, gpt-4-0125-preview, translated from English
date: 2024-02-22 17:30:10.233314-07:00
description: "C\xF3mo hacerlo: Un entorno virtual es un directorio autocontenido que\
  \ contiene todos los ejecutables necesarios para usar los paquetes que un proyecto\u2026"
lastmod: '2024-03-13T22:44:58.612199-06:00'
model: gpt-4-0125-preview
summary: "Un entorno virtual es un directorio autocontenido que contiene todos los\
  \ ejecutables necesarios para usar los paquetes que un proyecto Python necesitar\xED\
  a."
title: Iniciando un nuevo proyecto
weight: 1
---

## Cómo hacerlo:


### Crear un Entorno Virtual
Un entorno virtual es un directorio autocontenido que contiene todos los ejecutables necesarios para usar los paquetes que un proyecto Python necesitaría. Es aconsejable crear un entorno virtual para cada proyecto para evitar conflictos entre las dependencias del proyecto. Usa el módulo `venv`, que es parte de la biblioteca estándar de Python.

```shell
# Reemplaza 'myproject' con el nombre de tu proyecto
python3 -m venv myproject-env
```

Para activar el entorno virtual:

En Windows:
```shell
myproject-env\Scripts\activate.bat
```

En Unix o MacOS:
```shell
source myproject-env/bin/activate
```

Salida de Ejemplo (la salida puede variar ligeramente dependiendo del SO):
```shell
(myproject-env) $
```

### Instalando Paquetes
Usa `pip`, el instalador de paquetes para Python, para instalar, actualizar y eliminar paquetes. Aquí te mostramos cómo puedes instalar una biblioteca de terceros popular, `requests`, para hacer solicitudes HTTP:

```shell
pip install requests
```

Salida de Ejemplo:
```shell
Collecting requests
  Downloading requests-2.25.1-py2.py3-none-any.whl (61 kB)
     |████████████████████████████████| 61 kB 1.3 MB/s
Installing collected packages: requests
Successfully installed requests-2.25.1
```

### Configurando la Estructura del Proyecto
Un proyecto típico de Python podría verse algo así:

```
myproject/
│
├── myproject-env/    # Entorno virtual
├── docs/             # Documentación
├── tests/            # Pruebas unitarias e integración
│   └── __init__.py
├── myproject/        # Código fuente del proyecto 
│   ├── __init__.py
│   └── main.py
├── setup.py          # Archivo de configuración del proyecto
└── README.md         # Resumen del proyecto
```

### Crea Tu Primer Programa
Crea un archivo `main.py` dentro del directorio `myproject`. Aquí tienes un ejemplo de un programa simple:

```python
# myproject/myproject/main.py
def greet(name):
    return f"Hola, {name}!"

if __name__ == "__main__":
    print(greet("Mundo"))
```

Ejecuta tu programa:

```shell
python myproject/main.py
```

Salida de Ejemplo:
```shell
Hola, Mundo!
```

### Usa un Marco de Trabajo para Proyectos Mayores
Para proyectos más grandes, especialmente aplicaciones web, marcos como Django o Flask son invaluables. Aquí te mostramos cómo instalar Flask y crear una simple aplicación web "Hola, Mundo":

```shell
pip install Flask
```

Crea un archivo `app.py` con el siguiente contenido:

```python
# app.py
from flask import Flask
app = Flask(__name__)

@app.route("/")
def hello_world():
    return "<p>Hola, Mundo!</p>"

if __name__ == "__main__":
    app.run(debug=True)
```

Ejecuta la aplicación Flask:

```shell
flask run
```

Salida de Ejemplo:
```shell
 * Running on http://127.0.0.1:5000/ (Press CTRL+C to quit)
```

Navega a `http://127.0.0.1:5000/` en tu navegador web, y deberías ver el mensaje "¡Hola, Mundo!".
