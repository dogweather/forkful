---
title:                "Fish Shell: Extrayendo subcadenas"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas de texto puede ser una tarea muy útil y eficiente en la programación. Por ejemplo, si necesitas obtener parte de una dirección de correo electrónico, una fecha o información específica de un texto más largo, extraer substrings es la mejor manera de hacerlo.

## Cómo

El lenguaje Fish Shell tiene algunas funciones muy útiles para extraer subcadenas de texto de manera sencilla. Veamos algunos ejemplos:

```
Fish Shell - Comando:              Descripción:                    Ejemplo:
string sub <string> which           Devuelve posición de la        string sub "Hola Mundo" which o
                                   primera ocurrencia de una      "Mundo" which     # Devuelve 5
                                   cadena
string sub <string> 0 2             Devuelve una subcadena         string sub "Hola Mundo" 0 2 o
                                   desde una posición dada hasta  "Hola"           # Devuelve "Ho"
                                   otra indicada
string sub <string> 3               Devuelve la subcadena          string sub "Hola Mundo" 3 o
                                   desde una posición dada hasta  "a Mundo"        # Devuelve "a Mundo"
                                   el final del texto
string sub <string> 'palabra' 1     Devuelve una subcadena         string sub "Hola Mundo" 'nombre' 1 o
                                   desde una posición dada hasta  "Mundo"          # Devuelve "Mundo"
                                   la siguiente ocurrencia de
                                   una palabra dada
```

Puedes usar estos comandos directamente en tu terminal de Fish Shell o integrarlos en tus scripts de programación para obtener subcadenas de texto de manera rápida y eficiente.

## Deep Dive

Si quieres profundizar más en el tema de extraer subcadenas de texto, aquí hay algunos detalles que pueden serte de ayuda:

- Puedes usar comillas simples o dobles alrededor de las palabras si tienen espacios o caracteres especiales en ellas.
- Si no especificas una posición inicial o final en el comando `string sub`, se asume que es el principio o el final del texto, respectivamente.
- Puedes usar variables en lugar de escribir la cadena directamente en el comando. Esto puede ser útil si quieres extraer una subcadena de manera dinámica en tus scripts.

¡Con estos detalles en cuenta, estarás listo para extraer subcadenas de texto como un profesional!

## Ver también

- [Documentación de Fish Shell sobre el comando "string sub"](https://fishshell.com/docs/3.1/cmds/string.html#string-sub)
- [Tutorial interactivo de Fish Shell sobre la extracción de subcadenas](https://rootnroll.com/d/extrayendo-subcadenas-de-texto-con-el-comando-string-sub-en-fish-shell/)