---
title:                "Conversión de una cadena de texto a minúsculas"
aliases: - /es/fish-shell/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:21.426434-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversión de una cadena de texto a minúsculas"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Convertir un string a minúsculas significa cambiar todas las letras de mayúsculas a minúsculas. Los programadores hacen esto para normalizar texto, facilitando comparaciones y búsquedas sin diferencias por mayúsculas o minúsculas.

## Cómo hacerlo:

En Fish Shell, puedes usar `string to-lower` para convertir strings a minúsculas. Aquí tienes un ejemplo sencillo:

```Fish Shell
echo "Hola, MUNDO!" | string to-lower
```

Salida:

```Fish Shell
hola, mundo!
```

Y es así de simple. Usa el comando en tus scripts o en la línea de comandos para obtener resultados rápidos.

## Análisis Más Profundo

Históricamente, los shells en sistemas Unix-like han proporcionado distintas herramientas para manipular texto, como `awk`, `sed`, y `tr`. Fish Shell, en su enfoque moderno, trae su propio conjunto de utilidades de cadena sencillas y potentes.

Alternativamente, podrías usar el comando `tr`:

```Fish Shell
echo "Hola, MUNDO!" | tr '[:upper:]' '[:lower:]'
```

Sin embargo, `string to-lower` es más legible y específico de Fish, lo que hace que tu código sea más claro y mantenible.

En cuanto a la implementación, Fish utiliza funciones de la biblioteca C para la conversión de caracteres, asegurando compatibilidad y velocidad.

## Ver También

- Documentación oficial de Fish Shell sobre cadenas de texto (en inglés): https://fishshell.com/docs/current/cmds/string.html
- Tutorial de manipulación de texto en Fish Shell (en inglés): https://fishshell.com/docs/current/tutorial.html#tut_strings
