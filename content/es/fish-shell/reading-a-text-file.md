---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Leer un archivo de texto implica extraer y interpretar la información contenida en un archivo de texto plano. Los programadores lo hacen para manipular datos, configurar sistemas, analizar resultados y mucho más. 

## Cómo se hace:

En Fish Shell, se puede leer un archivo de texto con el comando `cat`, `less`, o `more`. Aquí unos ejemplos:

```fish
# Leer el contenido completo del archivo
cat mi_archivo.txt 

# Leer el archivo página por página
less mi_archivo.txt

# Similar a less, pero muestra la pantalla completa de texto a la vez
more mi_archivo.txt 
```
La salida del código será el contenido del archivo de texto en cuestión.

## En Detalle:

Historia: Fish Shell surgió como una alternativa más amigable, interactiva y fácil de usar a los tradicionales bash y sh.

Alternativas: Existen otras formas de leer archivos en diferentes lenguajes de programación, como `fread()` en C, `StreamReader` en C# o `FileReader` en Java.

Detalles de implementación: Cuando leemos un archivo de texto en Fish, el programa carga el archivo en memoria y luego lo procesa línea por línea. Para archivos más grandes, sería mejor usar `less` o `more` para evitar sobrecargar la memoria.

## Ver También:

Puedes ampliar tus conocimientos en las siguientes fuentes:
-  Documentación oficial de Fish Shell: [https://fishshell.com/docs/3.1/index.html](https://fishshell.com/docs/3.1/index.html)
- Tutorial en Video de Fish Shell: [https://www.youtube.com/watch?v=Dozyp8Q8h-Y](https://www.youtube.com/watch?v=Dozyp8Q8h-Y)