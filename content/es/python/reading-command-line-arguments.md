---
title:    "Python: Leyendo argumentos de línea de comando"
keywords: ["Python"]
---

{{< edit_this_page >}}

# ¿Por qué deberías leer argumentos de línea de comandos en Python?

Los argumentos de línea de comandos son una forma útil de interactuar con un programa de Python directamente desde la terminal. Al leer y utilizar argumentos de línea de comandos, puedes personalizar la ejecución de tu programa y realizar tareas específicas sin tener que modificar el código fuente. A continuación, te mostramos cómo hacerlo en Python.

## Cómo hacerlo

1. Importa el módulo `argparse` en tu programa Python:
```Python
import argparse
```
2. Define los argumentos de línea de comandos que deseas utilizar, indicando su nombre y tipo de dato:
```Python
parser = argparse.ArgumentParser(description='Programa para calcular el área de un triángulo')
parser.add_argument('base', type=int, help='Longitud de la base del triángulo')
parser.add_argument('altura', type=int, help='Altura del triángulo')
```
3. Lee los argumentos introducidos por el usuario utilizando el método `parse_args()`:
```Python
args = parser.parse_args()
```
4. Utiliza los argumentos en tu programa de la forma que desees. Por ejemplo, para calcular el área del triángulo con los argumentos introducidos:
```Python
area = (args.base * args.altura) / 2
print('El área del triángulo es:', area)
```
5. Ejecuta tu programa desde la terminal, proporcionando los argumentos requeridos:
```
python mi_programa.py 10 5
```
Output:
```
El área del triángulo es: 25
```

## Profundizando

La función `argparse.ArgumentParser()` acepta varios parámetros opcionales que te permiten personalizar aún más tus argumentos de línea de comandos. Algunos de los parámetros más útiles son:

- `description`: una breve descripción del programa que se incluirá en la ayuda.
- `add_argument()`: permite agregar argumentos personalizados con su nombre, tipo de dato y opciones adicionales, como ayuda y valores predeterminados.
- `parse_args()`: además de utilizar `args = parser.parse_args()` para leer los argumentos introducidos, también se puede utilizar `parser.parse_known_args()` para permitir argumentos desconocidos.
- `set_defaults()`: te permite establecer valores predeterminados para tus argumentos si no se proporcionan en la línea de comandos.

Puedes encontrar información completa sobre los diferentes parámetros y métodos de la función `argparse` en la documentación oficial de Python.

## Ver también

- [Documentación oficial de Python sobre `argparse`](https://docs.python.org/es/3/library/argparse.html)
- [Tutorial en español sobre cómo leer argumentos de línea de comandos en Python](https://platzi.com/blog/argparse-command-line-python/)
- [Ejemplos de uso de `argparse` en Python](https://pymotw.com/2/argparse/)

¡Ahora que sabes cómo leer argumentos de línea de comandos en Python, puedes utilizarlos en tus programas para hacerlos más interactivos y versátiles! ¡Diviértete programando y sigue aprendiendo!