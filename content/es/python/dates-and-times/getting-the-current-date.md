---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:24.024513-07:00
description: "Obtener la fecha actual en Python es una operaci\xF3n b\xE1sica para\
  \ muchas aplicaciones, tales como el registro de actividades, an\xE1lisis de datos\
  \ y la toma de\u2026"
lastmod: '2024-03-13T22:44:58.623434-06:00'
model: gpt-4-0125-preview
summary: "Obtener la fecha actual en Python es una operaci\xF3n b\xE1sica para muchas\
  \ aplicaciones, tales como el registro de actividades, an\xE1lisis de datos y la\
  \ toma de decisiones basada en tiempo."
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
