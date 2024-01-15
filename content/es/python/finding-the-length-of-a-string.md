---
title:                "Encontrando la longitud de una cadena"
html_title:           "Python: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué alguien querría encontrar la longitud de una cadena?

En la programación, encontrar la longitud de una cadena de texto es una tarea común y útil. Puede ser necesario para validar la entrada de un usuario, para realizar operaciones específicas en una cadena, o simplemente para imprimir información en un formato específico. Entender cómo encontrar la longitud de una cadena es una habilidad básica en Python y es vital para el desarrollo de habilidades de programación más avanzadas.

## Cómo hacerlo

En Python, podemos encontrar la longitud de una cadena utilizando la función `len()`, que devuelve el número de caracteres en una cadena dada. Veamos algunos ejemplos:

```python
cadena = "¡Hola, mundo!"
print(len(cadena)) # Output: 13

otra_cadena = "12345"
print(len(otra_cadena)) # Output: 5

vacía = ""
print(len(vacía)) # Output: 0
```

En el primer ejemplo, la cadena tiene 13 caracteres, incluyendo espacios y la coma. En el segundo ejemplo, la cadena es una serie de números y también tiene una longitud de 5. Finalmente, en el último ejemplo, la cadena está vacía y, por lo tanto, tiene una longitud de 0.

También podemos utilizar la función `len()` en otras estructuras de datos en Python, como listas y tuplas, para encontrar su longitud. Por ejemplo:

```python
lista = [1, 2, 3, 4, 5]
print(len(lista)) # Output: 5

tupla = (1, "a", True)
print(len(tupla)) # Output: 3
```

## Profundizando

La función `len()` en realidad llama al método `__len__()` del objeto que se pasa como argumento. Este método se puede sobrescribir en clases personalizadas para devolver una longitud personalizada para ese objeto. Por ejemplo:

```python
class MiClase:
  def __init__(self, mi_lista):
    self.mi_lista = mi_lista
  
  def __len__(self):
    return len(self.mi_lista) + 10

lista = [1, 2, 3]
mi_objeto = MiClase(lista)

print(len(mi_objeto)) # Output: 13
```

Además, la función `len()` solo es compatible con objetos que tienen un método `__len__()`. Si intentamos llamar a `len()` en un objeto sin ese método, recibiremos un error.

## Ver también

- [Documentación oficial de Python sobre la función len()](https://docs.python.org/es/3.9/library/functions.html#len)
- [Tutorial de Python para principiantes](https://pythonista.io/cursos/py101/operaciones-basicas/longitud-de-cadenas-y-secuencias)
- [Más información sobre funciones y métodos en Python](https://www.w3schools.com/python/python_functions.asp)