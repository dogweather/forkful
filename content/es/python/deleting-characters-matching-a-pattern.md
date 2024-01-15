---
title:                "Borrando caracteres que coinciden con un patrón"
html_title:           "Python: Borrando caracteres que coinciden con un patrón"
simple_title:         "Borrando caracteres que coinciden con un patrón"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

#¿Por qué borrar caracteres que coinciden con un patrón?

Borrar caracteres que coinciden con un patrón puede ser una tarea útil en la programación para limpiar textos o datos, eliminar errores o simplemente para simplificar un código. Al seguir leyendo, aprenderás a realizar esta acción en Python.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en Python, utilizaremos la función `re.sub()` del módulo de expresiones regulares. Primero, importamos el módulo con la siguiente línea de código:

```python
import re
```

A continuación, definiremos la cadena de texto en la que queremos borrar los caracteres y el patrón que queremos que coincida. Por ejemplo, si queremos eliminar todos los espacios y guiones bajos de una cadena, podemos escribir lo siguiente:

```python
texto = "hola - mundo"
patron = r"[\s_]"
```

En este caso, el patrón `[\s_]` coincide con cualquier espacio (representado por `\s`) o guión bajo (representado por `_`). Podemos utilizar la función `re.sub()` para reemplazar todas las coincidencias con una cadena vacía:

```python
resultado = re.sub(patron, "", texto)
print(resultado)
```

El resultado sería: `holamundo`.

## Profundizando

La función `re.sub()` también ofrece la posibilidad de utilizar una función como argumento en lugar de una cadena. Esta función se aplica a cada coincidencia y su valor de retorno se utiliza como reemplazo. Por ejemplo, si queremos convertir todos los caracteres en mayúsculas en una cadena, podemos utilizar la función `upper()` de la siguiente manera:

```python
texto = "hola mundo"
patron = r"[a-z]"
resultado = re.sub(patron, lambda x: x.group(0).upper(), texto)
print(resultado)
```

El resultado sería: `HOLA MUNDO`.

Además, también podemos utilizar grupos de captura en el patrón para guardar las coincidencias y utilizarlas en la función de reemplazo. Por ejemplo, si queremos reemplazar todas las letras "a" seguidas de un número con un punto, podemos hacer lo siguiente:

```python
texto = "a1, a2, a3, a4"
patron = r"a(\d)"
resultado = re.sub(patron, lambda x: "." + x.group(1), texto)
print(resultado)
```

El resultado sería: `.1, .2, .3, .4`.

## Ver también

- [Documentación de `re` en Python](https://docs.python.org/es/3/library/re.html)
- [Tutorial sobre expresiones regulares en Python](https://realpython.com/regex-python/)
- [Módulo `re` para expresiones regulares en Python (en inglés)](https://docs.python.org/3/library/re.html)