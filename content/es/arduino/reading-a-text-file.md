---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

# ¿Qué y por qué?
La lectura de un archivo de texto en la programación es básicamente leer un archivo que contiene texto y utilizar esa información en nuestro código. Los programadores lo hacen para poder almacenar datos externamente y acceder a ellos fácilmente en sus programas.

# ¿Cómo?
```
Arduino leer archivo de texto
```
El código anterior muestra cómo podemos leer un archivo de texto en Arduino. Primero, definimos una variable de tipo File que contendrá el archivo que queremos leer. Luego, usamos la función ```open()``` para abrir el archivo y la función ```readString()``` para leer el texto del archivo. Finalmente, imprimimos el texto en el monitor serial con la función ```println()```.

Ejemplo de salida:
```
Hola, este es un archivo de texto.
```

# Profundizando
La lectura de archivos de texto en la programación ha existido desde los primeros lenguajes de programación. Sin embargo, con el avance de la tecnología, también hay otras formas de almacenar datos, como bases de datos y servicios en la nube. A pesar de esto, la lectura de archivos de texto sigue siendo una forma rápida y sencilla de almacenar y acceder a datos.

# Ver también
- [Función open() de Arduino](https://www.arduino.cc/reference/en/language/functions/files/fileopen/)
- [Función readString() de Arduino](https://www.arduino.cc/reference/en/language/functions/files/filereadstring/)
- [Más información sobre archivos de texto en Arduino](https://www.teachmemicro.com/reading-and-writing-data-in-arduino/)