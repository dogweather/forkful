---
title:                "Elixir: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

¡Hola a todos los programadores de Elixir!

En esta publicación de blog, hablaremos sobre cómo escribir un archivo de texto en Elixir. Si eres nuevo en este lenguaje de programación o simplemente quieres aprender una nueva forma de escribir y almacenar información, ¡sigue leyendo!

## ¿Por qué escribir un archivo de texto?

Escribir un archivo de texto puede ser útil en muchas situaciones. Puede ser que quieras guardar los datos generados por tu programa, o tal vez quieras crear un archivo de configuración para tu aplicación. También puede ser una forma sencilla de guardar información que puedas leer y editar fácilmente más tarde. En resumen, escribir un archivo de texto te permite guardar y acceder a información importante de manera sencilla y eficiente.

## Cómo hacerlo

Para escribir un archivo de texto en Elixir, usaremos la función `File.write/2`. Esta función toma dos argumentos: la ruta del archivo y el contenido que deseas escribir en él. Veamos un ejemplo:

```Elixir
File.write("mi_archivo.txt", "Este es el contenido de mi archivo.")
```

Si ejecutas este código, se creará un archivo llamado "mi_archivo.txt" en la misma ubicación donde se encuentre tu programa. Puedes cambiar la ruta del archivo a cualquier ubicación que desees, siempre y cuando tengas permiso para escribir en esa carpeta.

Si deseas agregar más contenido al archivo en lugar de sobrescribirlo, puedes usar la función `File.append/2` de la misma manera que `File.write/2`.

## Profundizando

Ahora que sabemos cómo escribir un archivo de texto, hablemos de algunos detalles más avanzados. Si deseas escribir en un archivo que ya existe, puedes usar `File.open/2` para abrir y escribir en él. También puedes especificar el modo de escritura, como "w" para sobrescribir o "a" para agregar contenido al final del archivo.

Además, puedes escribir un archivo de texto desde una lista de cadenas usando la función `Enum.join/2`. Esta función tomará una lista de cadenas y las unirá en una cadena separada por el delimitador que elijas. Por ejemplo:

```Elixir
File.write("mi_archivo.txt", Enum.join(["Primera línea", "Segunda línea"], "\n"))
```

Esto creará un archivo con dos líneas de texto, separadas por un salto de línea.

## Ver también

Si deseas profundizar aún más en la escritura de archivos de texto en Elixir, te recomendamos verificar la documentación oficial y practicar con diferentes ejemplos. También puedes explorar otras funciones relacionadas, como `File.read/1` para leer archivos de texto y `File.write!/2` para escribir archivos con control de errores.

¡Esperamos que esta publicación te haya dado una visión general útil sobre cómo escribir archivos de texto en Elixir! ¡Feliz programación!