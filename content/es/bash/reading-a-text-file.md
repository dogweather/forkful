---
title:                "Leyendo un archivo de texto"
html_title:           "Bash: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Leer un archivo de texto en Bash es simplemente el acto de acceder al contenido de un archivo de texto y mostrarlo en la pantalla. Los programadores a menudo necesitan leer archivos de texto para obtener información o realizar tareas específicas con el contenido del archivo.

## Cómo:
Para leer un archivo de texto en Bash, podemos usar el comando ```cat```. Por ejemplo, ```cat file.txt``` mostrará el contenido de un archivo de texto llamado "file.txt" en la pantalla. También podemos usar el operador de redirección ```<``` para pasar el contenido de un archivo de texto a otro comando. Por ejemplo, ```wc -l < file.txt``` contará el número de líneas en el archivo "file.txt".

## Profundizando:
Leer archivos de texto en Bash ha sido una función básica desde los inicios de Unix. Además de usar el comando ```cat```, también podemos usar comandos como ```head``` y ```tail``` para mostrar solo una parte de un archivo. Alternativamente, también podemos escribir scripts más complejos usando lenguajes de programación como Python o Perl para leer y procesar archivos de texto.

## Ver También:
- [Bash Scripting Tutorial](https://ryanstutorials.net/bash-scripting-tutorial/)
- [The Unix Shell](https://swcarpentry.github.io/shell-novice/)
- [The Linux Command Line](http://www.linuxcommand.org/tlcl.php)