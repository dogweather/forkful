---
title:                "Escribiendo un archivo de texto"
html_title:           "Elixir: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir un archivo de texto en Elixir?

Muchas veces, cuando estamos aprendiendo un nuevo lenguaje de programación, queremos explorar sus funciones y capacidades. Una forma de hacerlo es escribiendo un archivo de texto en Elixir, ya que nos permite practicar la sintaxis del lenguaje y aplicar los conceptos básicos que hemos aprendido.

## Cómo hacerlo

Para escribir un archivo de texto en Elixir, primero debemos abrir una terminal o un editor de código y seguir los siguientes pasos:

1. Crear una nueva carpeta para nuestro proyecto: ```Elixir mkdir mi_proyecto```
2. Ingresar en la carpeta creada: ```Elixir cd mi_proyecto```
3. Crear un nuevo archivo con extensión ```.ex```: ```Elixir touch mi_archivo.ex```
4. Abrir el archivo en nuestro editor de código y empezar a escribir nuestro código en Elixir.

Algunos ejemplos de código que podemos utilizar para escribir un archivo de texto en Elixir son los siguientes:

```
# Crear un archivo nuevo
file = File.open("mi_archivo.txt", [:write])

# Escribir una línea de texto en el archivo
IO.write(file, "¡Hola mundo!")

# Cerrar el archivo
File.close(file)
```

Otro ejemplo, utilizando la función ```File.write```:

```
# Crear un archivo nuevo y escribir dos líneas de texto
File.write("mi_otro_archivo.txt", "Esta es la primera línea\nEsta es la segunda línea")
```

Al ejecutar estos ejemplos en la terminal, podremos ver que se ha creado el archivo de texto con el contenido que queríamos escribir.

## Profundizando en la escritura de archivos de texto

Además de las funciones mencionadas anteriormente, Elixir cuenta con una gran cantidad de herramientas para trabajar con archivos de texto. Algunas de ellas son:

- ```File.read``` para leer archivos de texto.
- ```File.exists?``` para verificar si un archivo existe.
- ```File.rename``` para cambiar el nombre de un archivo.

También podemos utilizar los módulos ```IO``` y ```IO.AnsiColor``` para imprimir y dar formato al texto que escribamos en nuestros archivos.

No es necesario memorizar todas estas funciones, lo importante es comprender cómo funcionan y cómo podemos utilizarlas en nuestros proyectos.

## Ver también

- [Documentación de Elixir sobre archivos](https://hexdocs.pm/elixir/File.html)
- [Ejemplos de escritura de archivos en Elixir](https://elixir-lang.org/getting-started/io-and-the-file-system.html#writing-to-files)