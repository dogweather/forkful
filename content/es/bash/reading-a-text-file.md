---
title:                "Lectura de un archivo de texto"
html_title:           "Bash: Lectura de un archivo de texto"
simple_title:         "Lectura de un archivo de texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué leer un archivo de texto?

Si eres un programador o un usuario de Linux, es muy probable que en algún momento necesites leer un archivo de texto en la línea de comandos. Ya sea para revisar un documento, leer un archivo de configuración o extraer datos de un archivo, saber cómo leer un archivo de texto en Bash es una habilidad esencial.

## Cómo hacerlo

La forma más sencilla de leer un archivo de texto en Bash es usando el comando `cat`. Este comando imprimirá el contenido completo del archivo en la línea de comandos. Por ejemplo, si queremos leer el archivo "ejemplo.txt", podemos escribir lo siguiente en la terminal:

```Bash
cat ejemplo.txt
```

Esto imprimirá el contenido del archivo en la terminal. Si el archivo es muy grande, puede ser útil usar la opción `less` en lugar de `cat`. Esto nos permitirá desplazarnos por el archivo con las teclas de flecha y la tecla "q" para salir.

```Bash
less ejemplo.txt
```

Si queremos guardar el contenido del archivo en una variable, podemos usar la redirección de entrada (`<`) y el comando `read`. Por ejemplo, si queremos almacenar el contenido del archivo "ejemplo.txt" en la variable `texto`, podemos hacer lo siguiente:

```Bash
read texto < ejemplo.txt
```

## Profundizando

Hay varios comandos y herramientas que puedes utilizar para leer un archivo de texto en Bash. Algunas opciones son: `head` para mostrar las primeras líneas del archivo, `tail` para mostrar las últimas líneas, `grep` para buscar patrones específicos en el archivo, entre otros.

Además, Bash también nos permite utilizar expresiones regulares para filtrar y procesar el contenido de un archivo de texto. Esto puede ser muy útil si estamos trabajando con datos estructurados en un archivo.

## Vea también

- [10 comandos básicos de Bash que deberías conocer](https://dev.to/lucis/10-bash-comandos-que-deberas-conocer-2p2j)
- [Cómo trabajar con archivos y directorios en Bash](https://linuxize.com/post/bash-check-if-file-exists/)
- [La guía completa de expresiones regulares en Bash](https://www.regular-expressions.info/posix.html)