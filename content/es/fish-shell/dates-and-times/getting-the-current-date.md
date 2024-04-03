---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:21.651043-07:00
description: "C\xF3mo hacerlo: Fish Shell utiliza comandos externos como `date` para\
  \ obtener la fecha actual, ofreciendo flexibilidad para formatear la salida seg\xFA\
  n sea\u2026"
lastmod: '2024-03-13T22:44:59.512043-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell utiliza comandos externos como `date` para obtener la fecha actual,\
  \ ofreciendo flexibilidad para formatear la salida seg\xFAn sea necesario."
title: Obteniendo la fecha actual
weight: 29
---

## Cómo hacerlo:
Fish Shell utiliza comandos externos como `date` para obtener la fecha actual, ofreciendo flexibilidad para formatear la salida según sea necesario. Así es cómo usarlo:

```fish
# Mostrar la fecha actual en el formato predeterminado
echo (date)

# Ejemplo de salida: Mié 25 Oct 2023 15:42:03 BST
```

Para personalizar el formato de la fecha, puedes usar la opción `+` seguida de especificadores de formato:

```fish
# Mostrar la fecha actual en formato AAAA-MM-DD
echo (date "+%Y-%m-%d")

# Ejemplo de salida: 2023-10-25
```

Para tareas más complejas, como trabajar con marcas de tiempo o realizar aritmética de fechas, Fish Shell se apoya en herramientas externas como `date` debido a su naturaleza de scripting. Aquí hay un ejemplo de cómo obtener la marca de tiempo UNIX actual:

```fish
# Obtener la marca de tiempo UNIX actual
echo (date "+%s")

# Ejemplo de salida: 1666710123
```

Y para añadir un día a la fecha actual usando `date`:

```fish
# Añadir un día a la fecha actual
echo (date -d "+1 day" "+%Y-%m-%d")

# Ejemplo de salida: 2023-10-26
```

Nota: Los ejemplos usan opciones del comando `date` que funcionan con GNU coreutils. Las opciones pueden variar en otros entornos como macOS, que utiliza el comando date de BSD por defecto. Siempre consulta `date --help` o la página del manual para detalles específicos de tu entorno.
