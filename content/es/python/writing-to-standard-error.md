---
title:    "Python: Escribiendo en el error estándar"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir a la salida de error?

Escribir a la salida de error es una práctica común en la programación de Python. Esta técnica se utiliza para mostrar mensajes de error o información de depuración durante la ejecución del programa. Al escribir a la salida de error, podemos separar claramente los mensajes de error de los mensajes de salida estándar, lo que facilita la identificación y resolución de problemas en nuestro código.

## Cómo hacerlo

Para escribir a la salida de error en Python, utilizamos el método `sys.stderr.write()`. Este método acepta un mensaje como parámetro y lo escribe en la salida de error. Veamos un ejemplo:

```python
import sys

# escribir un mensaje de error
sys.stderr.write("Ha ocurrido un error en el programa")

# escribir un mensaje de depuración
sys.stderr.write("La variable x tiene el valor: " + str(x))
```

La salida de este código se verá así:

```python
Ha ocurrido un error en el programa
La variable x tiene el valor: 5
```

Podemos notar que ambos mensajes están separados y se escriben en la salida de error en lugar de la salida estándar.

Otra forma de escribir a la salida de error es utilizando el módulo `logging` de Python. Este módulo nos permite manejar los mensajes de error de una manera más avanzada, como agregar información de fecha y hora, nivel de severidad, etc. Veamos un ejemplo:

```python
import logging

# configurar el manejador de errores
logging.basicConfig(filename='errores.log', level=logging.ERROR)

# escribir un mensaje de error
logging.error("Ha ocurrido un error en el programa")

# escribir un mensaje de depuración
logging.debug("La variable x tiene el valor: " + str(x))
```

En este caso, los mensajes se escribirán en un archivo llamado "errores.log" en lugar de la salida de error estándar.

## Profundizando

Escribir mensajes a la salida estándar puede ser útil para fines de depuración, pero es importante no abusar de esta técnica. Es recomendable utilizarla solo en casos de errores o para información de depuración crítica, ya que puede afectar el rendimiento de nuestro programa.

También es importante tener en cuenta que cuando ejecutamos nuestro programa desde la línea de comandos, los mensajes de error se muestran en rojo para facilitar su identificación. Sin embargo, si estamos ejecutando el programa desde un entorno integrado de desarrollo (IDE), los mensajes de error pueden mezclarse con la salida estándar y se pueden perder.

## Ver también

- [Documentación oficial de Python sobre `sys.stderr`](https://docs.python.org/es/3/library/sys.html#sys.stderr)
- [Tutorial de Real Python sobre el módulo `logging`](https://realpython.com/python-logging/)
- [Página de la comunidad de Python en español](https://comunidad.python.org.ar/)