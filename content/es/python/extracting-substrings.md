---
title:    "Python: Extrayendo subcadenas"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas es una habilidad útil en la programación de Python ya que nos permite manipular y obtener información específica de una cadena de texto. Esto es especialmente útil en proyectos de procesamiento de lenguaje natural o manejo de grandes conjuntos de datos.

## Cómo hacerlo

Para extraer una subcadena de una cadena de texto en Python, podemos utilizar el método `substring()` o la sintaxis de rebanado `string[índice_inicio:índice_fin]`. Veamos un ejemplo de cada método:

````Python
# Método substring - Extraer los primeros 5 caracteres de una cadena
cadena = "Hola mundo"
subcadena = cadena.substring(0,5)
print(subcadena) # Salida: Hola

# Sintaxis de rebanado - Extraer los últimos 5 caracteres de una cadena
cadena = "Hola mundo"
subcadena = cadena[7:12]
print(subcadena) # Salida: mundo
````

También podemos usar expresiones regulares para extraer subcadenas que cumplan con un patrón específico. Por ejemplo, si queremos encontrar todas las palabras con más de 5 letras en una cadena, podemos usar la función `findall()` del módulo `re`:

````Python
import re

cadena = "Bienvenidos al mundo de la programación"
patron = r"\b\w{6,}\b" # Expresión regular para encontrar palabras con más de 5 letras
subcadenas = re.findall(patron, cadena)
print(subcadenas) # Salida: ['Bienvenidos', 'programación']
````

## Profundizando

Al extraer subcadenas en Python, es importante tener en cuenta que los índices se cuentan desde 0 y que el índice final especificado en la sintaxis de rebanado no se incluirá en la subcadena resultante. También podemos utilizar números negativos en la sintaxis de rebanado para contar desde el final de la cadena.

Además, si tratamos de acceder a un índice que está fuera del rango de la cadena, obtendremos un error `IndexError`.

## Ver también

- Documentación oficial de Python sobre `str.substring()`: https://docs.python.org/es/3/library/stdtypes.html#str.substring
- Tutorial de programación en Python por SoloLearn: https://www.sololearn.com/Course/Python/
- Ejemplos de expresiones regulares para Python: https://regexone.com/references/python