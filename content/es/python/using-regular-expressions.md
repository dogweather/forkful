---
title:    "Python: Utilizando expresiones regulares"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué utilizar expresiones regulares en Python

Las expresiones regulares son una poderosa herramienta para buscar y manipular texto en un programa de Python. Si estás trabajando con datos de texto, como correos electrónicos o documentos, las expresiones regulares te ayudarán a encontrar patrones específicos y automatizar tareas tediosas. ¡Vamos a ver cómo utilizarlas!

## Cómo utilizar expresiones regulares en Python

Para utilizar expresiones regulares en Python, primero debes importar la librería `re`:

```Python
import re
```

Luego, puedes usar la función `search()` para buscar un patrón específico en una cadena de texto. Por ejemplo, si quieres encontrar todos los correos electrónicos en un texto, puedes utilizar el siguiente código:

```Python
text = "Mi correo electrónico es ejemplo@email.com. ¿Tú tienes un correo electrónico?"
results = re.search("[a-zA-Z0-9]+@[a-zA-Z0-9]+\.[a-z]+", text)
print(results.group())
```

Este código buscará una cadena de texto que contenga cualquier combinación de letras, números y símbolos seguida de una @ y otra combinación de letras, números y símbolos, seguida de un punto y una extensión de dominio (como .com o .es). El resultado mostrado en la consola será `ejemplo@email.com`.

Otro ejemplo común es utilizar expresiones regulares para validar entradas de usuario en un formulario. Por ejemplo, si quieres asegurarte de que un usuario ingrese un número de teléfono en el formato correcto, puedes utilizar el siguiente código:

```Python
phone_number = input("Ingresa tu número de teléfono (solo números): ")
results = re.match("[0-9]{3}-[0-9]{3}-[0-9]{4}", phone_number)
if results:
    print("Número de teléfono válido")
else:
    print("Número de teléfono inválido")
```

Este código buscará una cadena de texto que contenga exactamente tres números, un guión, otros tres números, otro guión y finalmente cuatro números. Si la entrada del usuario sigue este formato, se mostrará `Número de teléfono válido`, de lo contrario, se mostrará `Número de teléfono inválido`.

## Profundizando en el uso de expresiones regulares en Python

Las expresiones regulares tienen una amplia variedad de reglas y símbolos que pueden ser combinados para buscar patrones complejos en un texto. Algunos de estos símbolos incluyen:

- **`[]`**: Utilizado para especificar un conjunto de caracteres. Por ejemplo, `[a-z]` coincidirá con cualquier letra minúscula.
- **`{}`**: Indica un número específico de ocurrencias de un patrón. Por ejemplo, `[0-9]{2}` coincidirá con dos números consecutivos.
- **`*`**: Indica que el patrón anterior puede repetirse cero o más veces. Por ejemplo, `[a-z]*` coincidirá con una cadena de cualquier longitud que contenga solo letras minúsculas.

Además, hay muchos métodos más avanzados para usar expresiones regulares en Python, como `sub()` para reemplazar patrones en una cadena de texto o `findall()` para encontrar todas las coincidencias en una cadena.

¡No tengas miedo de experimentar y probar diferentes patrones para obtener los resultados deseados! Puedes aprender más sobre expresiones regulares en la [documentación oficial de Python](https://docs.python.org/es/3/library/re.html).

## Ver también

- [Tutorial de expresiones regulares en Python](https://www.freecodecamp.org/news/python-regex-tutorial/)
- [Expresiones regulares en la práctica](https://realpython.com/regex-python/)
- [Guía de referencia rápida para expresiones regulares en Python](https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial)