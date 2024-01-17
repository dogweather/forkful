---
title:                "Concatenando cadenas"
html_title:           "Fish Shell: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Concatenar cadenas de texto es cuando se combinan dos o más cadenas para formar una sola. Los programadores lo hacen para crear mensajes complejos o manipular datos de manera eficiente.

## Cómo:
Los bloques de código de ```Fish Shell``` se pueden usar para concatenar cadenas de texto de la siguiente manera:

```
set nombre "Juan"
set apellido "Pérez"
echo "¡Hola $nombre $apellido!"
```

El resultado de este código sería "¡Hola Juan Pérez!". También se pueden concatenar cadenas directamente en la línea de comando, por ejemplo: `echo "¡Hola" "mundo!"` resultaría en "¡Hola mundo!".

## Profundizando:
La concatenación de cadenas de texto es una técnica común en la programación y se puede realizar en otros lenguajes además de Fish Shell. Algunas alternativas a la concatenación incluyen el uso de `printf` o `sprintf` en C, o `join` en Python.

En términos de implementación, la concatenación de cadenas puede ser más o menos eficiente dependiendo del lenguaje y las funciones utilizadas. Por ejemplo, en Fish Shell, utilizar un solo argumento en `echo` es más eficiente que concatenar múltiples cadenas.

## Ver también:
- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/cmds/echo.html)
- [Artículo de concatenación de cadenas en C en GeeksforGeeks](https://www.geeksforgeeks.org/concatenate-strings-in-c-3-different-ways/)