---
title:    "Python: Concatenando cadenas"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por qué

Las cadenas de texto son uno de los tipos de datos más comunes en la programación. Son secuencias de caracteres que pueden ser utilizadas para almacenar información o para ser mostradas en una interfaz de usuario. A veces, es necesario combinar varias cadenas en una sola. Esto se conoce como concatenación de cadenas y es una habilidad básica de programación que es esencial para muchos proyectos.

## Cómo hacerlo

En Python, se puede concatenar una o más cadenas utilizando el operador `+`. Veamos un ejemplo:

```python
nombre = "Ana"
apellido = "García"
nombre_completo = nombre + " " + apellido
print(nombre_completo)
```
La salida de este código sería: `Ana García`

En este ejemplo, utilizamos el operador `+` para unir las variables `nombre` y `apellido` junto con un espacio en blanco para crear `nombre_completo`. También es posible concatenar cadenas con valores numéricos, como en el siguiente ejemplo:

```python
edad = 35
mensaje = "Tengo " + str(edad) + " años."
print(mensaje)
```
La salida de este código sería: `Tengo 35 años.`

Aquí utilizamos la función `str()` para convertir el valor numérico de `edad` en una cadena antes de concatenarla con el resto del mensaje.

También es posible concatenar más de dos cadenas utilizando el operador `+` varias veces, como en este ejemplo:

```python
saludo = "Hola "
sujeto = "amigo"
puntuacion = "!"
print(saludo + sujeto + puntuacion)
```
La salida sería: `Hola amigo!`

## Profundizando

Además del operador `+`, Python también ofrece otros métodos para concatenar cadenas de texto. Uno de ellos es utilizando el método `format()`. Este método permite insertar valores en una cadena utilizando marcadores de posición. Veamos un ejemplo:

```python
animal = "gato"
color = "negro"
caracteristicas = "Es un {} {}".format(color, animal)
print(caracteristicas)
```
La salida sería: `Es un negro gato`

Aquí, los `{}` actúan como marcadores de posición para que los valores de `color` y `animal` se inserten en la cadena en ese orden.

También es posible utilizar el método `format()` con variables y valores numéricos, lo que lo hace muy versátil. Puedes encontrar más información sobre este método en la documentación oficial de Python.

## Ver también

- [Documentación oficial de Python sobre cadenas de texto](https://docs.python.org/es/3/tutorial/introduction.html#strings)
- [Tutorial de concatenación de cadenas en Python](https://realpython.com/python-string-formatting/)
- [Ejercicios de concatenación de cadenas en Python](https://www.w3resource.com/python-exercises/string/python-data-type-string-exercise-1.php)