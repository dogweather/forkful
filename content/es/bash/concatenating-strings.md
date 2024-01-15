---
title:                "Uniendo cadenas de texto"
html_title:           "Bash: Uniendo cadenas de texto"
simple_title:         "Uniendo cadenas de texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

La concatenación de cadenas es una técnica común en la programación, especialmente en Bash. Se utiliza para combinar varias cadenas de texto en una sola, lo que puede ser útil para la creación de mensajes dinámicos, formateo de texto y manipulación de datos.

## Cómo hacerlo
```Bash
# Ejemplo 1: Usando el operador de concatenación '+'
nombre="Juan"
apellido="Pérez"
echo $nombre$apellido
# Output: JuanPérez

# Ejemplo 2: Usando la interpolación de variables
nombre="María"
apellido="González"
mensaje="Hola, mi nombre es ${nombre} ${apellido}."
echo $mensaje
# Output: Hola, mi nombre es María González.

# Ejemplo 3: Combinando varios valores en una sola cadena
numeros="1, 2, 3"
letras="a, b, c"
echo "Los números son ${numeros} y las letras son ${letras}."
# Output: Los números son 1, 2, 3 y las letras son a, b, c.
```

## Profundizando
La concatenación de cadenas se realiza mediante el uso de operadores o mediante la interpolación de variables. Además de los ejemplos anteriores, también se pueden combinar múltiples cadenas en una sola línea utilizando el operador de concatenación '+'. Por ejemplo: `echo "Hola" + "mundo"` resultará en "Holamundo".

También es posible concatenar cadenas almacenadas en variables usando el operador '+='. Por ejemplo: `nombre="Ana"; nombre+="María"; echo $nombre` resultará en "AnaMaría".

Es importante tener en cuenta que al concatenar cadenas, no se agregan espacios entre ellas automáticamente. Por lo tanto, es necesario incluirlos explícitamente en la cadena si se desean. Además, la concatenación no es exclusiva de cadenas, también se pueden combinar valores numéricos, booleanos, etc.

## Ver también
- [Shell Scripting Tutorial](https://www.shellscript.sh/)
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Bash Tips](https://www.linuxnix.com/category/bash-tips/)