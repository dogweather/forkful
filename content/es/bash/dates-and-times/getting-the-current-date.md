---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:42.056527-07:00
description: "Recuperar la fecha actual en Bash implica usar comandos integrados para\
  \ mostrar la fecha y la hora en varios formatos. Los programadores utilizan esta\u2026"
lastmod: 2024-02-19 22:05:17.764229
model: gpt-4-0125-preview
summary: "Recuperar la fecha actual en Bash implica usar comandos integrados para\
  \ mostrar la fecha y la hora en varios formatos. Los programadores utilizan esta\u2026"
title: Obteniendo la fecha actual
---

{{< edit_this_page >}}

## Qué y Por Qué?
Recuperar la fecha actual en Bash implica usar comandos integrados para mostrar la fecha y la hora en varios formatos. Los programadores utilizan esta funcionalidad para tareas como poner marcas de tiempo en registros, programar tareas o simplemente como parte de sus scripts de información del sistema para rastrear cuándo se realizaron las acciones.

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
