---
title:                "Escribiendo un archivo de texto"
html_title:           "Bash: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

# ¿Qué y por qué?

Escribir un archivo de texto es una tarea común para los programadores en Bash. Consiste en crear un archivo que contenga texto plano, como un documento. Los programadores lo hacen para almacenar datos, configurar su entorno, o para ejecutar scripts que realicen tareas específicas.

# ¿Cómo hacerlo?

Para escribir un archivo de texto en Bash, utilizamos el comando ```echo```. En su forma más simple, escribimos ```echo "contenido del archivo de texto" > archivo.txt``` en la terminal. Esto creará un archivo llamado "archivo.txt" con el texto indicado.

Podemos agregar más contenido al archivo utilizando el símbolo ">>" en lugar de ">" en nuestro comando. Por ejemplo, si queremos agregar una línea adicional al archivo, escribimos ```echo "otra línea" >> archivo.txt```.

# Profundizando

La habilidad de escribir archivos de texto en Bash es una característica útil ya que permite a los programadores guardar y acceder a información de manera sencilla. Antes de Bash, los programadores tenían que usar programas externos como "notepad" o "nano" para escribir archivos de texto.

Existen otras formas de escribir archivos de texto en Bash, como "printf" o "cat", pero "echo" es el más común. Además, podemos utilizar el comando ```touch``` para crear un archivo vacío y luego escribir en él utilizando "echo".

# Ver también

- [Comando echo en Bash](https://linux.die.net/man/1/echo)
- [Uso de bash para crear un archivo de texto en Linux](https://www.howtogeek.com/107537/lesser-known-linux-commands-echo-touch-find/)