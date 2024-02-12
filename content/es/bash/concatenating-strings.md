---
title:                "Concatenación de cadenas de texto"
aliases:
- es/bash/concatenating-strings.md
date:                  2024-01-20T17:34:16.433471-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenación de cadenas de texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
Concatenar cadenas es simplemente combinar dos o más textos en uno solo. Los programadores lo hacen para manejar dinámicamente la información, como construir mensajes o comandos de manera eficiente.

## How to:
Concatenar en Bash es pan comido. Usamos `+` para sumar números, pero para unir palabras, pegamos variables o cadenas lado a lado. Aquí va:

```Bash
# Concatenando con variables
saludo="Hola, "
nombre="Mundo"
mensaje=${saludo}${nombre}
echo $mensaje  # Salida: Hola, Mundo

# Directamente en el echo
echo "Tech" "And" "Code"  # Salida: Tech And Code

# Con variables y texto fijo
prefijo="mega"
sufijo="genial"
echo ${prefijo}ultra${sufijo}  # Salida: mega-ultra-genial
```

## Deep Dive
La concatenación de cadenas es una de las primeras técnicas en el mundo de la programación, tan antigua como la propia Bash. Hay varias formas de hacerlo, pero en Bash la simplicidad gana. Sin operadores especiales, simplemente colocas elementos uno junto al otro.

Alternativas incluyen el uso de comandos externos como `awk` o `sed`, pero por eficiencia y claridad, pegar variables directamente es best practice. Ten en cuenta el contexto: si estás en medio de un bucle o tratando con texto que incluye caracteres especiales, querrás prestar más atención a cómo construyes tus cadenas.

## See Also
Para un buceo aún más profundo, échale un vistazo a estos enlaces:

- [GNU Bash manual](https://www.gnu.org/software/bash/manual/)
- [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/)
- [Stack Overflow: Concatenating strings in Bash](https://stackoverflow.com/questions/4181703/how-to-concatenate-string-variables-in-bash)
