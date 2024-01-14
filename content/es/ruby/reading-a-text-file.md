---
title:    "Ruby: Leyendo un archivo de texto"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

¡Hola programadores de Ruby! En este blog post, vamos a hablar sobre cómo leer un archivo de texto en sus scripts de Ruby. Si eres nuevo en Ruby o simplemente quieres aprender más sobre esta tarea, ¡sigue leyendo!

## Por qué
Antes de empezar a mostrarte cómo leer un archivo de texto en Ruby, es importante entender por qué esta habilidad es útil. Leer datos de un archivo es una parte crucial en el procesamiento de información y puede ser útil para muchas tareas de programación, como analizar registros, generar informes o incluso crear aplicaciones de lectura de archivos.

## Cómo hacerlo
Para comenzar, necesitas tener un archivo de texto que quieras leer y un editor de texto para escribir tu código Ruby. Una vez que tengas ambos, sigue estos pasos:

1. Abre tu editor de texto y crea un nuevo archivo.
2. Escribe el siguiente código para abrir y leer el archivo:

```Ruby
File.open("archivo.txt", "r") do |file|
    puts file.read
end
```
Este código abre el archivo "archivo.txt" en modo de lectura y utiliza el método `read` para imprimir su contenido en la consola. Puedes reemplazar "archivo.txt" con el nombre de tu archivo de texto.

3. Guarda tu archivo con una extensión .rb y ejecútalo mediante tu terminal o consola de comandos. Verás el contenido de tu archivo impreso en la consola.

¡Felicidades! Acabas de leer con éxito un archivo de texto en Ruby. Pero eso no es todo, también puedes utilizar otros métodos como `readlines`, que lee cada línea del archivo y las almacena en un array, o `each_line`, que itera sobre cada línea del archivo.

## Profundizando
Ahora que ya sabes cómo leer un archivo de texto en Ruby, permítenos profundizar un poco más. En Ruby, puedes usar el bloque de código `File.open` para leer los archivos. Este bloque se asegura de que el archivo se cierre automáticamente después de que se completen las operaciones de lectura, lo que es muy útil para evitar posibles errores.

Además, es importante tener en cuenta que cuando abres un archivo en modo de lectura, no puedes escribir o realizar ningún cambio en él. Si necesitas hacer cambios en el archivo, deberás abrirlo en modo de escritura (`"w"`) o modo de lectura-escritura (`"r+"`). También puedes especificar la ruta completa del archivo si no se encuentra en la misma ubicación que tu código.

## Ver también
- [Documentación de Ruby sobre manejo de archivos](https://ruby-doc.org/core-2.7.4/File.html)
- [Tutorial de Ruby de Codecademy](https://www.codecademy.com/learn/learn-ruby)
- [Ejemplos de archivos de texto para practicar](https://www.beej.us/guide/bgnet/examples/)

¡Esperamos que este blog post te haya ayudado a comprender mejor cómo leer archivos de texto en Ruby! Recuerda que la práctica hace al maestro, así que asegúrate de seguir practicando y experimentando con diferentes métodos para mejorar tus habilidades de programación en Ruby. ¡Hasta la próxima!