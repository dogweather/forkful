---
title:                "Bash: Leyendo un archivo de texto"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías leer un archivo de texto?

Si estás aprendiendo a programar en Bash, es importante comprender cómo leer y manipular archivos de texto. Esto te permitirá trabajar con datos de manera eficiente y automatizar tareas en tu sistema operativo. En este artículo, te explicaremos cómo leer un archivo de texto en Bash y algunas técnicas avanzadas para que puedas sacar el máximo provecho de esta habilidad.

## Cómo leer un archivo de texto en Bash

Para leer un archivo de texto en Bash, puedes utilizar el comando `while` junto con el comando `read`, como se muestra en el siguiente ejemplo:

```Bash
while IFS= read -r line; do
  echo "$line"
done < nombre_archivo.txt
```

Este código creará un bucle que leerá cada línea del archivo de texto y la imprimirá en la consola. Puedes personalizar el código según tus necesidades, como por ejemplo guardar cada línea en una variable para poder manipularla más tarde.

## Profundizando en la lectura de archivos de texto

Además de simplemente leer líneas de un archivo, Bash también te permite hacer cosas más avanzadas como buscar y filtrar datos específicos. Por ejemplo, puedes utilizar el comando `grep` para buscar una cadena de texto en un archivo o el comando `sed` para reemplazar ciertas palabras.

Otra técnica útil es utilizar el comando `awk` para analizar datos estructurados en un archivo de texto. Por ejemplo, si tienes un archivo CSV, puedes utilizar `awk` para separar los campos y manipularlos de manera individual.

## Ver también

- [Guía de Bash para principiantes](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Manipulación de archivos en Bash](https://www.lifewire.com/all-about-file-redirection-3867575)
- [Comandos básicos de Bash](https://towardsdatascience.com/bash-commands-with-practical-examples-773e3d03590a)