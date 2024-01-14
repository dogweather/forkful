---
title:    "Python: Buscando y reemplazando texto."
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por qué

A veces, cuando trabajamos en un proyecto de Python, nos encontramos con la necesidad de modificar texto en nuestros archivos. Esta tarea puede ser bastante tediosa si se tiene que hacer de forma manual. Afortunadamente, Python nos proporciona una forma sencilla de buscar y reemplazar texto en nuestros archivos de manera eficiente.

## Cómo Hacerlo

Para realizar una búsqueda y reemplazo de texto en Python, primero necesitamos importar el módulo `re` (de "regular expression" o expresiones regulares). Luego, utilizamos la función `sub()` para realizar el reemplazo.

Veamos un ejemplo práctico. Supongamos que tenemos un archivo llamado "datos.txt" que contiene la siguiente información:

```
Nombre: Ana
Edad: 25
```

Si queremos cambiar la edad de Ana a 26, podemos utilizar el siguiente código en Python:

```
import re

# Abrimos el archivo con la información
with open("datos.txt", "r") as f:
    data = f.read()
    
# Utilizamos la función `sub()` para reemplazar el texto
# Buscamos la cadena "Edad: 25" y la reemplazamos por "Edad: 26"
nueva_data = re.sub(r'Edad: 25', 'Edad: 26', data)

# Sobreescribimos el contenido del archivo con el nuevo texto
with open("datos.txt", "w") as f:
    f.write(nueva_data)

# Imprimimos el resultado
print(nueva_data)
```

La salida de este código sería:

```
Nombre: Ana
Edad: 26
```

Como se puede ver, el texto ha sido reemplazado correctamente.

## Profundizando

El módulo `re` de Python nos permite utilizar expresiones regulares para realizar búsquedas y remplazos de texto más complejos. Podemos utilizar patrones para buscar palabras o caracteres específicos, así como también utilizar otras funciones como `findall()` para encontrar todas las coincidencias en un texto.

Además, podemos utilizar la función `subn()` en lugar de `sub()` si deseamos obtener el número de reemplazos realizados en lugar del texto modificado.

Recomiendo explorar más acerca de las expresiones regulares y sus diferentes funciones para ampliar aún más tus habilidades de búsqueda y reemplazo de texto en Python.

## Ver También

- [Documentación oficial del módulo re de Python](https://docs.python.org/es/3.9/library/re.html)
- [Tutorial de expresiones regulares en Python](https://www.tutorialspoint.com/python/python_reg_expressions.htm)
- [Ejemplos prácticos y ejercicios de expresiones regulares en Python](https://www.geeksforgeeks.org/regular-expression-python/)