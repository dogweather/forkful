---
title:    "Python: Obteniendo la fecha actual"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Por qué obtener la fecha actual en Python

¡Bienvenido a mi blog para amantes de la programación en Python! En esta publicación, exploraremos cómo obtener la fecha actual en Python y por qué es una habilidad importante para tener en tu caja de herramientas de programación.

## Cómo hacerlo

En Python, hay varias formas de obtener la fecha actual. Una forma sencilla es utilizar la biblioteca incorporada `datetime` y la función `datetime.now()`. Aquí hay un ejemplo de cómo usarlo:

```python
import datetime

current_date = datetime.now()
print(current_date)
```

El resultado de este código sería la fecha y hora actuales en formato `año-mes-día hora:minuto:segundo`.

También puedes especificar el formato en el que deseas que se muestre la fecha. Por ejemplo, si solo quieres la fecha en formato `día/mes/año`, puedes usar el método `.strftime()` (abreviatura de "string format time"). Aquí hay un ejemplo de cómo hacerlo:

```python
import datetime

current_date = datetime.now().strftime('%d/%m/%Y')
print(current_date)
```

El resultado de este código sería la fecha actual en el formato `día/mes/año`.

## Profundizando en el tema

La biblioteca `datetime` también ofrece varias opciones para trabajar con fechas y horas en Python. Por ejemplo, puedes obtener solo el día, el mes o el año de la fecha actual utilizando los métodos `.day`, `.month` o `.year`, respectivamente.

Además, si necesitas realizar operaciones con fechas, como sumar o restar días a una fecha determinada, la biblioteca `datetime` también ofrece métodos para hacerlo. Por ejemplo, puedes usar `.timedelta()` para sumar o restar un número específico de días, horas o minutos a una fecha determinada.

Otra biblioteca útil para trabajar con fechas y horas en Python es `arrow`, que proporciona una sintaxis más concisa y fácil de entender para manejar fechas. Puedes instalarla usando el comando `pip install arrow`.

## Ver también

- [Documentación oficial de Python sobre el módulo `datetime`](https://docs.python.org/es/3/library/datetime.html)
- [Documentación oficial de Python sobre el módulo `arrow`](https://arrow.readthedocs.io/en/latest/)

Espero que esta publicación te haya ayudado a comprender cómo obtener la fecha actual en Python y cómo puedes usar este conocimiento para realizar operaciones con fechas en tus proyectos. ¡Nos vemos en la próxima publicación!