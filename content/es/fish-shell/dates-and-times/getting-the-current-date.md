---
title:                "Obteniendo la fecha actual"
aliases: - /es/fish-shell/getting-the-current-date.md
date:                  2024-02-03T19:09:21.651043-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obteniendo la fecha actual"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?
Obtener la fecha actual en programación es una tarea fundamental que te permite recuperar y manipular los datos de fecha y hora del sistema. En tareas de script y automatización, es esencial para generar marcas de tiempo, programar tareas y crear registros.

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
