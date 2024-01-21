---
title:                "Interpolación de cadenas de texto"
date:                  2024-01-20T17:50:13.981080-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolación de cadenas de texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
La interpolación de cadenas es insertar valores de variables en medio de una cadena de texto. En Bash, los programamos para ahorrar tiempo y evitar errores, facilitando leer y mantener el código.

## Cómo hacerlo:
A continuación, unos ejemplos de cómo interpolamos cadenas en Bash:

```Bash
nombre="Mundo"
# Interpolación básica
echo "Hola, $nombre"

# Interpolación con llaves para claridad
puesto="profesor"
echo "Hola, ${nombre}, eres un gran ${puesto}."

# Ejemplo con comando subshell
echo "Tengo $(ls | wc -l) archivos en el directorio actual."
```

Salida esperada:
```
Hola, Mundo
Hola, Mundo, eres un gran profesor.
Tengo 42 archivos en el directorio actual.
```

## Detalles Profundos
La interpolación de cadenas ha sido una característica en los shells desde los tiempos del Bourne Shell. En Bash, la interpolación permite no solo la inclusión de variables, sino también la ejecución de comandos dentro de cadenas usando la sintaxis `$(comando)`. Esencialmente, Bash reemplaza la variable o el comando con su valor de salida. Alternativas como concatenar con `+` no existen en Bash como en otros lenguajes. Siempre ten en cuenta las comillas dobles para permitir la expansión de variables; las comillas simples no lo harán.

## Ver También
- GNU Bash Documentation: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/ 
- Bash String Manipulation Examples: https://linuxhint.com/bash_string_manipulation/