---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:42.056527-07:00
description: "C\xF3mo hacerlo: En Bash, el comando `date` es tu herramienta principal\
  \ para obtener la fecha y hora actuales. Aqu\xED hay algunos ejemplos de c\xF3mo\
  \ usarlo: 1.\u2026"
lastmod: '2024-03-13T22:44:59.257538-06:00'
model: gpt-4-0125-preview
summary: En Bash, el comando `date` es tu herramienta principal para obtener la fecha
  y hora actuales.
title: Obteniendo la fecha actual
weight: 29
---

## Cómo hacerlo:
En Bash, el comando `date` es tu herramienta principal para obtener la fecha y hora actuales. Aquí hay algunos ejemplos de cómo usarlo:

1. **Obtener la fecha y hora actuales en el formato predeterminado:**

```bash
date
```

*Ejemplo de salida:*
```
Wed Apr 5 14:22:04 PDT 2023
```

2. **Personalizar el formato de salida:** Puedes especificar el formato de salida usando los especificadores de formato `+%`. Por ejemplo, para mostrar la fecha en formato YYYY-MM-DD:

```bash
date "+%Y-%m-%d"
```

*Ejemplo de salida:*
```
2023-04-05
```

3. **Obtener el sello de tiempo UNIX actual:** El sello de tiempo UNIX es el número de segundos desde la Época Unix (1 de enero de 1970). Esto es útil para scripts que realizan cálculos basados en diferencias de tiempo.

```bash
date "+%s"
```

*Ejemplo de salida:*
```
1672877344
```

No se suelen utilizar bibliotecas de terceros populares para esta operación básica en Bash ya que el comando `date` integrado proporciona una funcionalidad completa. Sin embargo, para manipulaciones de fecha y hora más avanzadas, los programadores podrían usar otros lenguajes de programación o herramientas que ofrecen bibliotecas para aritmética y análisis de fechas, como el módulo `datetime` de Python.
