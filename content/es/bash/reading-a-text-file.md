---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La lectura de un archivo de texto es simplemente el proceso de recuperar datos de un archivo de texto en tu programa. Los programadores hacen esto para manipular, analizar o simplemente mostrar el contenido de un archivo.

## ¿Cómo hacerlo?

La lectura de un archivo de texto en Bash es franca. Utilizando el bucle `while` y la instrucción `read`, podemos iterar a través de cada línea en el archivo. Aquí está la plantilla básica:

```Bash
while read linea
do
  echo $linea
done < mi_archivo.txt
```
Si tienes un archivo llamado `mi_archivo.txt` con este contenido:
```
Hola mundo
Esto es Bash
```
El output producido estaría:
```
Hola mundo
Esto es Bash
```

## Análisis profundo

La lectura de archivos de texto ha sido una práctica común en la programación desde sus inicios. Sin embargo, Bash, lanzado en 1989, simplificó enormemente el proceso para los sistemas Unix.

Existen alternativas a `read`, por ejemplo, el comando `cat` puede usarse para leer archivos de texto. Sin embargo, tiene menos flexibilidad y control en comparación con `read`.

Aquí hay un pequeño detalle sobre cómo funciona esto: `read` lee una línea a la vez del archivo especificado que es redirigido usando `<`. Por cada iteración en el bucle `while`, la línea leída se asigna a la variable de la que se especificó (en nuestro caso, `linea`).

## Ver también

- [Bash Guide for Beginners] (https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Advanced Bash-Scripting Guide] (https://tldp.org/LDP/abs/html/)
- [Bash programming in Unix with Examples] (https://www.geeksforgeeks.org/bash-scripting/)