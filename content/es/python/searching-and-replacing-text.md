---
title:                "Python: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

Si eres programador o trabajas con grandes cantidades de texto, es probable que en algún momento necesites realizar cambios masivos en tus archivos. En lugar de hacerlos manualmente uno por uno, Python ofrece una manera más eficiente de hacerlo utilizando la función de búsqueda y reemplazo.

## Cómo

Para comenzar, necesitamos tener un archivo de texto en el que deseamos realizar los cambios. En este ejemplo, utilizaremos un archivo llamado "texto.txt" con el siguiente contenido:

```
Este es un texto de ejemplo que necesitamos modificar para insertar palabras nuevas.
Este archivo también contiene una línea que queremos eliminar por completo.
Finalmente, queremos reemplazar todas las letras "e" con la letra "a".
```

Para buscar y reemplazar texto en este archivo, utilizaremos la función `.replace()` en conjunto con el método `.read()` para leer el contenido del archivo. El siguiente código muestra cómo hacerlo:

```Python
with open('texto.txt', 'r') as archivo:
    contenido = archivo.read()
    
# Buscamos y reemplazamos la palabra "insertar" con "agregar"
nuevo_contenido = contenido.replace('insertar', 'agregar')
    
# Eliminamos la línea que contiene la palabra "eliminamos"
nuevo_contenido = nuevo_contenido.replace('eliminamos\n', '')
    
# Reemplazamos todas las letras "e" con la letra "a"
nuevo_contenido = nuevo_contenido.replace('e', 'a')

print(nuevo_contenido)
```

Al ejecutar este código, obtendremos el siguiente resultado:

```
Esta as un texto da axamplo que nacasitamos modifacar para insartar palabras nuavas.
Finalmanta, quaramos ramplazar todas las latras "a" con la letra "m".
```

Como se puede ver, todas las palabras "insertar" se han reemplazado por "agregar", la línea con la palabra "eliminamos" se eliminó por completo y todas las letras "e" se han reemplazado por "a".

## Deep Dive

La función de búsqueda y reemplazo en Python es una herramienta muy útil para realizar cambios masivos en archivos de texto. Además de los ejemplos mencionados anteriormente, también es posible buscar y reemplazar patrones específicos utilizando expresiones regulares.

Por ejemplo, si queremos reemplazar todas las palabras que comienzan con la letra "a" por "Python", podemos utilizar el siguiente código:

```Python
import re

patron = r'a\w+'

with open('texto.txt', 'r') as archivo:
    contenido = archivo.read()
    
nuevo_contenido = re.sub(patron, 'Python', contenido)

print(nuevo_contenido)
```

Al ejecutar este código, obtendremos el siguiente resultado:

```
Este es un texto de ejemplo que necesitamos modificar para Python palabras nuevas.
Esta archivo Python contiene una lúnea que queremos eliminar por completo.
Finalmente, queremos reemplazar todas las letras "e" con la letra "a".
```

En este ejemplo, utilizamos la librería `re` y el método `.sub()` para reemplazar todas las palabras que comienzan con la letra "a" con la palabra "Python".

## Ver También

- [Documentación de la función `.replace()` en Python](https://docs.python.org/es/3/library/stdtypes.html#str.replace)
- [Documentación de la librería `re` en Python](https://docs.python.org/es/3/library/re.html)
- [Tutorial de expresiones regulares en Python](https://www.programiz.com/python-programming/regex)