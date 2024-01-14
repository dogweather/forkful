---
title:                "Python: Calculando una fecha en el futuro o en el pasado"
simple_title:         "Calculando una fecha en el futuro o en el pasado"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##Por qué

Una de las tareas más comunes en la programación es calcular una fecha en el futuro o en el pasado. Esto puede ser útil para diversas aplicaciones, como el seguimiento de eventos o la planificación de tareas. Aprender a calcular estas fechas también te ayudará a comprender mejor la manipulación de fechas y tiempos en Python.

##Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado en Python, primero debemos importar el módulo datetime. Luego, utilizamos la función datetime.timedelta(), que toma como argumentos los días, horas, minutos y segundos que queremos añadir o restar a la fecha actual. Veamos un ejemplo de cómo calcular la fecha y hora actuales y después sumarle 5 días:

```python
# Importar el módulo datetime
import datetime

# Obtener la fecha y hora actuales
fecha_actual = datetime.datetime.now()

# Sumar 5 días a la fecha y hora actuales
nueva_fecha = fecha_actual + datetime.timedelta(days=5)

print("La nueva fecha es:", nueva_fecha)
```
La salida de este código sería: `La nueva fecha es: 2021-12-25 10:28:57.686841`

También podemos restar una cantidad de tiempo para obtener una fecha en el pasado. Por ejemplo, si queremos obtener la fecha hace 3 meses, utilizamos un valor negativo en la función timedelta():

```python
# Obtener la fecha y hora actuales
fecha_actual = datetime.datetime.now()

# Restar 3 meses a la fecha y hora actuales
fecha_pasada = fecha_actual - datetime.timedelta(days=3*30)

print("La fecha hace 3 meses era:", fecha_pasada)
```
La salida sería `La fecha hace 3 meses era: 2021-09-27 10:28:57.686841`

##Profundizando

El módulo datetime en Python también incluye otras funciones útiles para trabajar con fechas y tiempos, como la función date(), que nos permite obtener solo la fecha sin la hora, y la función time(), que nos permite obtener solo la hora sin la fecha.

También podemos crear nuestras propias fechas utilizando el constructor datetime(), especificando el año, mes, día, hora, minuto y segundo:

```python
# Crear una fecha específica
fecha = datetime.datetime(2021, 12, 31, 11, 59, 59)

print("La fecha creada es:", fecha)
```
La salida sería `La fecha creada es: 2021-12-31 11:59:59`

En resumen, calcular fechas en el futuro o en el pasado en Python es una tarea muy útil y sencilla. Con el uso del módulo datetime y sus funciones, podemos realizar cálculos precisos y personalizar nuestras fechas según nuestras necesidades.

## Vea también
- Documentación oficial del módulo datetime de Python: https://docs.python.org/es/3/library/datetime.html
- Tutorial sobre manipulación de fechas y tiempos en Python: https://realpython.com/python-datetime/#calculating-deltas-with-timedelta
- Ejemplos prácticos de cálculo de fechas en Python: https://www.geeksforgeeks.org/python-datetime-timedelta-function/