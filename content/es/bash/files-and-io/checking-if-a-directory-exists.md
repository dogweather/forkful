---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:40.344673-07:00
description: "C\xF3mo hacerlo: En su n\xFAcleo, Bash te permite comprobar la existencia\
  \ de un directorio usando declaraciones condicionales y el operador `-d`. A\u2026"
lastmod: '2024-03-13T22:44:59.261452-06:00'
model: gpt-4-0125-preview
summary: "En su n\xFAcleo, Bash te permite comprobar la existencia de un directorio\
  \ usando declaraciones condicionales y el operador `-d`."
title: Comprobando si un directorio existe
weight: 20
---

## Cómo hacerlo:
En su núcleo, Bash te permite comprobar la existencia de un directorio usando declaraciones condicionales y el operador `-d`. A continuación, se muestra un ejemplo sencillo que demuestra cómo realizar esta comprobación.

```bash
if [ -d "/ruta/al/directorio" ]; then
    echo "El directorio existe."
else
    echo "El directorio no existe."
fi
```

Salida de muestra (si el directorio existe):
```
El directorio existe.
```

Salida de muestra (si el directorio no existe):
```
El directorio no existe.
```

Para scripts más complejos, es común combinar la comprobación con otras operaciones, como crear el directorio si no existe:

```bash
DIR="/ruta/al/directorio"
if [ -d "$DIR" ]; then
    echo "$DIR existe."
else
    echo "$DIR no existe. Creando ahora..."
    mkdir -p "$DIR"
    echo "$DIR creado."
fi
```

Salida de muestra (si el directorio no existe y luego se crea):
```
/ruta/al/directorio no existe. Creando ahora...
/ruta/al/directorio creado.
```

Aunque Bash en sí proporciona herramientas robustas para tales comprobaciones, no existen bibliotecas de terceros populares específicamente para esta tarea, ya que los comandos nativos de Bash son completamente capaces y eficientes para la validación de presencia de directorios.
