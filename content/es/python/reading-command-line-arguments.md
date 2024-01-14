---
title:    "Python: Leyendo argumentos de línea de comando"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Por qué leer argumentos de línea de comandos en Python?

Antes de sumergirnos en cómo leer argumentos de línea de comandos en Python, es importante entender por qué es una habilidad útil para cualquier programador. Leer argumentos de línea de comandos nos permite interactuar con nuestro programa directamente desde la terminal, lo que facilita la pruebas y depuración de nuestro código. Además, nos permite crear aplicaciones más dinámicas que puedan ser personalizadas por el usuario a través de los argumentos de la línea de comandos.

## Cómo hacerlo

Para leer argumentos de línea de comandos en Python, utilizamos el módulo `sys` y su función `argv`. Por ejemplo, si queremos obtener el primer argumento que dio el usuario al ejecutar nuestro programa, podemos usar `sys.argv[1]`, ya que el primer elemento de la lista `argv` contiene el nombre del archivo que está siendo ejecutado.

```Python
import sys

# Obtener el primer argumento
primer_argumento = sys.argv[1]

print(f"El primer argumento es: {primer_argumento}")
```

Si el usuario ejecuta nuestro programa con el comando `python programa.py argumento1`, veremos en la terminal la siguiente salida:

```
El primer argumento es: argumento1
```

Podemos también obtener más de un argumento utilizando un bucle `for` y el resto de los elementos de la lista `argv`.

```Python
import sys

# Obtener todos los argumentos
for argumento in sys.argv[1:]:
    print(argumento)
```

## Profundizando

Al leer argumentos de línea de comandos, es importante validar y limpiar los datos antes de utilizarlos en nuestro código. También es posible utilizar el módulo `argparse` para crear argumentos con nombre y manejar errores de forma más organizada y profesional. Además, este módulo permite agregar descripciones y documentación a nuestros argumentos, lo que hace que nuestro programa sea más amigable para el usuario.

# Ver también

- [Documentación oficial de Python sobre el módulo `sys`](https://docs.python.org/es/3/library/sys.html)
- [Documentación oficial de Python sobre el módulo `argparse`](https://docs.python.org/es/3/library/argparse.html)
- [Tutorial de Real Python sobre cómo usar argumentos de línea de comandos en Python](https://realpython.com/command-line-interfaces-python-argparse/)