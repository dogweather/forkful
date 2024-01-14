---
title:    "Python: Calculando una fecha en el futuro o pasado"
keywords: ["Python"]
---

{{< edit_this_page >}}

## ¿Por qué calcular una fecha en el futuro o pasado?

Calcular fechas en el futuro o pasado es una habilidad importante en la programación ya que nos permite automatizar procesos y ahorrar tiempo. Además, es una herramienta útil para muchas aplicaciones, como calendarios, recordatorios y planificadores.

## Cómo hacerlo

Para calcular una fecha en el futuro o pasado en Python, podemos utilizar la biblioteca de tiempo integrada llamada "datetime". Primero, debemos importarla en nuestro código utilizando la siguiente línea de código:

````Python
import datetime
````

Luego, podemos utilizar la función "timedelta" para especificar el número de días, semanas, meses o años que queremos avanzar o retroceder en una fecha. Por ejemplo, si queremos calcular la fecha dentro de una semana a partir de hoy, podemos usar el siguiente código:

````Python
hoy = datetime.datetime.today()
una_semana = datetime.timedelta(days=7)
fecha_futura = hoy + una_semana
print(fecha_futura)
````

Esto nos dará como resultado la fecha exacta dentro de una semana a partir de hoy. También podemos restar un "timedelta" de una fecha para obtener una fecha anterior. Por ejemplo, si queremos calcular la fecha hace un mes a partir de hoy, podemos usar el siguiente código:

````Python
hoy = datetime.datetime.today()
un_mes = datetime.timedelta(weeks=4)
fecha_pasada = hoy - un_mes
print(fecha_pasada)
````

Esto nos dará la fecha exacta de hace un mes a partir de hoy.

## Más información

Calcular fechas en el futuro o pasado en Python también nos permite manejar fechas en diferentes formatos, como horas, minutos y segundos. También podemos utilizar otros métodos de la biblioteca "datetime" para seleccionar fechas específicas e incluso comparar fechas. Es una herramienta muy versátil que puede ser utilizada en una variedad de proyectos y aplicaciones.

## Ver también

- Documentación oficial de la biblioteca "datetime" en Python: https://docs.python.org/es/3/library/datetime.html
- Cálculo de fechas en Python utilizando "timedelta": https://www.geeksforgeeks.org/python-timedelta-function/
- Tutoriales y ejemplos sobre fechas y tiempos en Python: https://www.programiz.com/python-programming/datetime