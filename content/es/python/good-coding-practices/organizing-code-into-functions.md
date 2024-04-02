---
date: 2024-01-26 01:11:44.189681-07:00
description: "Organizar el c\xF3digo en funciones consiste en desglosar tu c\xF3digo\
  \ en bloques reutilizables con fines espec\xEDficos. Lo hacemos para que el c\xF3\
  digo sea m\xE1s\u2026"
lastmod: '2024-03-13T22:44:58.617976-06:00'
model: gpt-4-1106-preview
summary: "Organizar el c\xF3digo en funciones consiste en desglosar tu c\xF3digo en\
  \ bloques reutilizables con fines espec\xEDficos. Lo hacemos para que el c\xF3digo\
  \ sea m\xE1s\u2026"
title: "Organizando c\xF3digo en funciones"
weight: 18
---

## ¿Qué y por qué?
Organizar el código en funciones consiste en desglosar tu código en bloques reutilizables con fines específicos. Lo hacemos para que el código sea más limpio, más fácil de leer, depurar y actualizar.

## Cómo hacerlo:
Digamos que estás escribiendo un script para calcular el cuadrado y el cubo de un número. Sin funciones, es un lío de repeticiones:

```Python
num = 4
cuadrado = num * num
cubo = num * num * num
print(f"Cuadrado: {cuadrado}, Cubo: {cubo}")

num = 5
cuadrado = num * num
cubo = num * num * num
print(f"Cuadrado: {cuadrado}, Cubo: {cubo}")
```
Salida:
```
Cuadrado: 16, Cubo: 64
Cuadrado: 25, Cubo: 125
```

Con funciones, es más ordenado:

```Python
def cuadrado(n):
    return n * n

def cubo(n):
    return n ** 3

num = 4
print(f"Cuadrado: {cuadrado(num)}, Cubo: {cubo(num)}")

num = 5
print(f"Cuadrado: {cuadrado(num)}, Cubo: {cubo(num)}")
```
Salida:
```
Cuadrado: 16, Cubo: 64
Cuadrado: 25, Cubo: 125
```

## Análisis profundo
En los viejos tiempos, cuando los programas eran simples, podías arreglártelas con solo escribir una lista de instrucciones. Pero a medida que el software se volvía más complejo, los desarrolladores se dieron cuenta de que reescribían el mismo código una y otra vez. Hola, funciones—bloques de código reutilizables que realizan una única acción.

Las alternativas a las funciones incluyen clases (agrupando funciones con los datos en los que operan) y código en línea (inteligencia justo donde la necesitas, pero arriesgado para tareas complejas). En cuanto a la implementación, el truco no es solo crear funciones, sino hacer que estas hagan una cosa bien—piensa en el principio de responsabilidad única. Las funciones también deberían ser idealmente sin estado, lo que significa que no hay sorpresas con los datos que entran o salen.

## Ver también
- Los tutoriales oficiales de Python sobre funciones: https://docs.python.org/3/tutorial/controlflow.html#defining-functions
- 'Código Limpio' de Robert C. Martin, para principios sobre cómo escribir funciones limpias.
- 'Refactorización: Mejorando el diseño del código existente' de Martin Fowler, que incluye ejemplos de organización de código.
