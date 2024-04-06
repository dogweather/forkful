---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:24.024513-07:00
description: "C\xF3mo hacerlo: **Usando la librer\xEDa est\xE1ndar `datetime`:** El\
  \ m\xF3dulo `datetime` en la librer\xEDa est\xE1ndar de Python provee clases para\
  \ manipular fechas y\u2026"
lastmod: '2024-04-05T21:53:59.980143-06:00'
model: gpt-4-0125-preview
summary: "**Usando la librer\xEDa est\xE1ndar `datetime`:** El m\xF3dulo `datetime`\
  \ en la librer\xEDa est\xE1ndar de Python provee clases para manipular fechas y\
  \ horas."
title: Obteniendo la fecha actual
weight: 29
---

## Cómo hacerlo:
**Usando la librería estándar `datetime`:**

El módulo `datetime` en la librería estándar de Python provee clases para manipular fechas y horas. Para obtener la fecha actual, puedes usar el método `date.today()`.

```python
from datetime import date

today = date.today()
print(today)  # Salida: AAAA-MM-DD (ej., 2023-04-05)
```

**Formateo de Tiempo:**

Si requieres la fecha actual en un formato diferente, el método `strftime` permite especificar un formato de fecha personalizado:

```python
from datetime import date

today = date.today()
formatted_date = today.strftime('%B %d, %Y')  # Formato de ejemplo: "Abril 05, 2023"
print(formatted_date)
```

**Usando `pendulum` para más flexibilidad (una librería de terceros popular):**

`Pendulum` es una librería de terceros que ofrece un enfoque más intuitivo para tratar con fechas y horas en Python. Extiende las funcionalidades estándar de datetime y simplifica la gestión de zonas horarias, entre otras características.

Primero, asegúrate de haber instalado `pendulum` via pip:

```shell
pip install pendulum
```

Luego, para obtener la fecha actual:

```python
import pendulum

today = pendulum.now().date()
print(today)  # Salida: AAAA-MM-DD (ej., 2023-04-05)
```

Con `pendulum`, el formateo también es sencillo y similar al enfoque de `strftime`:

```python
import pendulum

today = pendulum.now()
formatted_date = today.to_formatted_date_string()  # Formato predeterminado: "Abr 5, 2023"
print(formatted_date)
```
