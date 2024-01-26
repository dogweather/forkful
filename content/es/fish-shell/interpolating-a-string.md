---
title:                "Interpolación de cadenas de texto"
date:                  2024-01-20T17:50:36.479815-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolación de cadenas de texto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Interpolar una cadena consiste en insertar variables o expresiones en medio de una cadena de texto. Programadores lo hacen para construir mensajes dinámicos sin complicarse concatenando trozos de texto.

## Cómo hacerlo:
En Fish Shell, interpolar es sencillo. Usa las comillas dobles para interpretar variables dentro de una cadena. Si la variable es `username` y vale `Juan`, aquí tienes un ejemplo:

```Fish Shell
set username Juan
echo "Hola, $username. ¿Qué tal tu día?"
```

Salida:

```
Hola, Juan. ¿Qué tal tu día?
```

## Buceo Profundo
Históricamente, la interpolación de cadenas ha facilitado la generación de texto. En otros shells, como Bash, requieres un símbolo especial (`$`) delante de la variable. Fish es más legible porque puedes omitirlo dentro de comillas dobles. Aunque Fish no soporta brace expansion como Bash, ofrece otras maneras de manipular datos con su sintaxis simple y directa. La implementación usa expansión de variables y permite la combinación de texto estático y variables para crear salida personalizada y flexible.

## Ver También
- Documentación oficial de la expansión de variables en Fish: [https://fishshell.com/docs/current/#variable-expansion](https://fishshell.com/docs/current/#variable-expansion)
- Una comparación general de las shells de Unix: [https://en.wikipedia.org/wiki/Comparison_of_command_shells](https://en.wikipedia.org/wiki/Comparison_of_command_shells)
