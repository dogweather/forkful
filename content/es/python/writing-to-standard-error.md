---
title:                "Python: Escribir a la salida de error estándar"
simple_title:         "Escribir a la salida de error estándar"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir a la salida estándar de error en Python?

Al escribir programas en Python, es importante tener una forma de manejar y visualizar errores en el código. La salida estándar de error, también conocida como `stderr`, proporciona una forma de imprimir información de errores en la terminal. Esto puede ser útil para el desarrollo y la depuración de código, ya que permite identificar y solucionar problemas más fácilmente.

## Cómo hacerlo

El proceso para escribir a `stderr` en Python es bastante simple. Primero, debemos importar el módulo `sys` que nos proporciona acceso a `stderr`. Luego, utilizamos la función `write()` en `stderr` para imprimir nuestro mensaje de error en la consola.

```Python
import sys
sys.stderr.write("¡Error! No se puede dividir por cero.\n")
```

Esto imprimirá el mensaje de error en la terminal, seguido de una nueva línea.

```Texto
¡Error! No se puede dividir por cero.
```

También podemos utilizar la función `print()` con la opción `file` para imprimir directamente en `stderr`.

```Python
print("¡Error! No se puede dividir por cero.", file=sys.stderr)
```

## Profundizando

Cuando escribimos a `stderr`, en realidad estamos escribiendo en un objeto `Stream` que representa la salida estándar de error. Este objeto tiene métodos adicionales que nos permiten tener un mayor control sobre cómo se imprime la información.

Por ejemplo, podemos utilizar el método `flush()` para vaciar el buffer y forzar la impresión del mensaje inmediatamente.

```Python
sys.stderr.write("¡Error! No se puede dividir por cero.")
sys.stderr.flush()
```

También podemos cambiar la forma en que se manejan los errores en nuestro código mediante el uso de `sys.excepthook`. Este método nos permite definir una función que será llamada cuando se produzca una excepción en nuestro programa. De esta manera, podemos personalizar cómo se manejan y se muestran los errores.

```Python
def excepcion_personalizada(excepcion_tipo, excepcion_valor, traceback):
    sys.stderr.write("¡Ups! Algo salió mal.\n")
    sys.stderr.write("Tipo de excepción: {}\n".format(excepcion_tipo))
    sys.stderr.write("Valor de excepción: {}\n".format(excepcion_valor))

sys.excepthook = excepcion_personalizada

x = 10 / 0  # Esto generará una excepción que será manejada por nuestra función
```

## Ver también

- Tutorial de Python: Manejo de excepciones (https://www.python.org/dev/peps/pep-0352/)
- Documentación de Python: Módulo `sys` (https://docs.python.org/es/3/library/sys.html)
- Guía para principiantes de Python: Depuración de código (https://realpython.com/python-debugging-pdb/)

¡Esperamos que este artículo te haya sido útil para aprender a escribir a `stderr` en Python! Recuerda que la salida estándar de error es una herramienta útil para el desarrollo de código y siempre es importante tener en cuenta cómo manejar y visualizar los errores en nuestros programas. ¡Happy coding!