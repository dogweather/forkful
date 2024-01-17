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

## ¿Qué es y por qué hacerlo?
La concatenación de cadenas en Bash es simplemente la combinación de dos o más strings en uno solo. Los programadores hacen esto para crear mensajes dinámicos, construir URLs, entre otras cosas.

## Cómo hacerlo:
Una manera de concatenar cadenas en Bash es utilizando el operador de concatenación "++". Por ejemplo: 
```Bash
cadena1="Hola"
cadena2="mundo"
echo $cadena1$cadena2
```
Esto imprimirá "Holamundo" en la consola.

Otra forma de hacerlo es utilizando la función "printf" y especificando el formato de salida. Por ejemplo:
```Bash
cadena1="¡Hola"
cadena2="amigos!"
printf "%s %s" $cadena1 $cadena2
```
Esto imprimirá "¡Hola amigos!" en la consola.

## Detalles:
La concatenación de cadenas en Bash no siempre ha sido tan sencilla. En versiones anteriores, los programadores tenían que utilizar comandos complicados como "expr" o "tr" para lograrlo. Sin embargo, con el avance de la tecnología, se ha vuelto una tarea mucho más simple.

Otra opción para la concatenación de cadenas es utilizar la herramienta "sed" en lugar de Bash. Esto puede ser más eficiente para concatenar múltiples cadenas o en situaciones específicas.

## Ver también:
- [Documentación oficial de Bash](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion) 
- [Concatenar cadenas en Shell Script](https://tecadmin.net/concatenate-string-in-bash-script/)