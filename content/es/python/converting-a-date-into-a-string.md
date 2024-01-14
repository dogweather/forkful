---
title:    "Python: Convirtiendo una fecha en una cadena de caracteres"
keywords: ["Python"]
---

{{< edit_this_page >}}

## ¡Por qué!

En algún momento, mientras se está programando en Python, es posible que se encuentre con la necesidad de convertir una fecha en una cadena de texto. Esto puede ser necesario para imprimir la fecha en un formato específico o para utilizarla como parte de una cadena de búsqueda en una base de datos. A continuación, entraremos en más detalle sobre cómo hacer esto en Python.

## Cómo hacerlo

La forma más sencilla de convertir una fecha en una cadena en Python es utilizando el método `strftime()` del módulo `datetime`. Este método toma como argumento un formato de fecha específico y devuelve una cadena con la fecha en ese formato.

```Python
import datetime

# Crear un objeto de fecha
fecha = datetime.date(2021, 10, 28)

# Convertir la fecha en una cadena
cadena_fecha = fecha.strftime("%d/%m/%Y")

# Imprimir la cadena
print(cadena_fecha)
```

El resultado de este código sería `28/10/2021`, ya que hemos especificado que queremos la fecha en el formato de día/mes/año. Hay una gran variedad de formatos que se pueden utilizar en el método `strftime()`, así que asegúrate de revisar la documentación de Python si necesitas un formato específico.

También es posible convertir una fecha con hora en una cadena utilizando el método `strftime()`. En ese caso, se utiliza el formato de `strftime()` para especificar cómo quieres que se muestre la hora.

```Python
import datetime

# Crear un objeto de fecha con hora
fecha_hora = datetime.datetime(2021, 10, 28, 14, 30)

# Convertir la fecha con hora en una cadena
cadena_fecha_hora = fecha_hora.strftime("%d/%m/%Y %H:%M")

# Imprimir la cadena
print(cadena_fecha_hora)
```

Este código imprimiría `28/10/2021 14:30`, ya que hemos especificado que queremos la fecha en el formato de día/mes/año y la hora en formato de 24 horas.

## Profundizando

En Python, las fechas se almacenan como objetos de la clase `date` o `datetime` en el módulo `datetime`. Estos objetos tienen varios métodos que se pueden utilizar para manipular y formatear fechas.

En el ejemplo anterior, utilizamos el método `strftime()` para convertir una fecha en una cadena. Sin embargo, también hay un método llamado `strptime()` que se puede utilizar para leer una cadena y convertirla en un objeto de fecha. Ambos métodos utilizan los mismos códigos de formato, así que es importante tenerlos en cuenta al utilizarlos.

## Consultar también

- Documentación de Python sobre el módulo `datetime`: https://docs.python.org/es/3/library/datetime.html
- Tabla de códigos de formato para el método `strftime()`: https://docs.python.org/es/3/library/datetime.html#strftime-and-strptime-format-codes