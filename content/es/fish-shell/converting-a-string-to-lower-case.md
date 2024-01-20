---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

La conversión de una cadena a minúsculas en programación es el proceso de cambiar cualquier carácter en mayúsculas en un string a su correspondiente en minúsculas. Los programadores hacen esto para estandarizar los datos, lo que ayuda a evitar errores y problemas de coincidencia de texto.

## Cómo:

Para convertir un string a minúsculas en Fish Shell, usamos el comando `string lower`.

```fish
# String inicial
set miString "HOLA MUNDO"

# Conversión en string a minúsculas
set miString (string lower -q $miString)

# Imprimir el string
echo $miString
```

El resultado será:

```fish
hola mundo
```

## Un vistazo en profundidad:

**1. Contexto histórico:** La mayoría de los lenguajes de programación ofrecen funciones incorporadas para convertir caracteres a minúsculas. Fish Shell no es la excepción.

**2. Alternativas:** Si necesitas trabajar en bash en lugar de Fish Shell, podrías usar el comando `tr` para convertir un string a minúsculas tal y cómo se muestra:

```bash
echo "HOLA MUNDO" | tr '[:upper:]' '[:lower:]'
```

**3. Detalles de implementación:** Fish Shell ofrece una selección de funciones incorporadas, incluyendo `string lower`, la cual no solo es efectiva, sino también eficiente, ya que ha sido optimizada para su uso con este shell de línea de comandos.

## Ver También:

Para mayor información y orientación, visita los siguientes enlaces:
- Documentación de Fish Shell: [https://fishshell.com/docs/current/](https://fishshell.com/docs/current/)