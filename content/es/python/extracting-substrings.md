---
title:    "Python: Extrayendo subcadenas"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas es una habilidad esencial en la programación de Python, ya que nos permite manipular y obtener información específica de una cadena de texto. Esto es especialmente útil en la limpieza y organización de datos, y en el procesamiento de texto.

## Cómo hacerlo

Existen varias formas de extraer subcadenas en Python, pero una de las más comunes es utilizando el método `substring()` en una cadena de texto.

```Python
cadena = "¡Hola Mundo!"
subcadena = cadena.substring(1, 4)
print(subcadena)
```
El código anterior imprimirá "ola", ya que estamos extrayendo los caracteres desde la posición 1 hasta la 4 de la cadena original. Tener en cuenta que la posición inicial es 0 y no 1.

Otra forma de extraer subcadenas es utilizando la notación de "rebanadas" (`slicing notation`). En este caso, especificaremos la posición inicial y final, separadas por dos puntos.

```Python
cadena = "Python es increíble"
subcadena = cadena[0:6]
print(subcadena)
```

Este código imprimirá "Python", ya que estamos seleccionando desde la posición 0 hasta la 6 (no incluyendo la 6) de la cadena original.

También podemos utilizar números negativos para representar posiciones desde el final de la cadena.

```Python
cadena = "¡Bienvenidos a Python!"
subcadena = cadena[-7:-1]
print(subcadena)
```

Este código nos dará como resultado "Python", ya que estamos seleccionando desde la posición -7 hasta la -1 (no incluyendo la -1) a partir del final de la cadena.

## Profundizando

Además de la extracción básica de subcadenas, también podemos utilizar otras funciones y métodos para dividir cadenas en subcadenas más pequeñas. Por ejemplo, podemos utilizar el método `split()` para dividir una cadena en una lista de subcadenas, utilizando un separador específico.

```Python
cadena = "manzana, banana, pera, sandía"
subcadenas = cadena.split(", ")
print(subcadenas)
```

El resultado será una lista con las subcadenas "manzana", "banana", "pera" y "sandía".

También podemos utilizar expresiones regulares para definir patrones específicos y extraer subcadenas que cumplan con dichos patrones.

```Python
import re

cadena = "El código es 1234 y la contraseña es 5678"
patron = "\d{4}"
subcadenas = re.findall(patron, cadena)
print(subcadenas)
```

El resultado será una lista con las subcadenas "1234" y "5678", ya que estamos buscando secuencias de 4 dígitos en la cadena original.

## Ver también

- Documentación oficial de Python: https://www.python.org/
- Tutorial de subcadenas en W3Schools: https://www.w3schools.com/python/python_strings.asp
- Guía de expresiones regulares en Python: https://docs.python.org/3.8/howto/regex.html