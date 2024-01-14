---
title:    "Ruby: Escribiendo un archivo de texto."
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por qué escribir un archivo de texto en Ruby

Escribir un archivo de texto en Ruby es una práctica común y útil para guardar y manipular datos. Puede ser especialmente útil en aplicaciones web, donde se pueden guardar configuraciones, registros de usuarios o registros de actividad.

## Cómo hacerlo

Escribir un archivo de texto en Ruby es muy sencillo. Simplemente sigue estos pasos:

1. Abre un nuevo archivo utilizando `File.new()` y pasa como argumento el nombre y la extensión del archivo que deseas crear.
2. Dentro del bloque `do`, escribe el contenido que deseas guardar en el archivo utilizando `puts`.
3. Cierra el archivo utilizando `File.close()`.

```Ruby
# Crea un nuevo archivo de texto
archivo = File.new("mi_archivo.txt", "w")

# Escribe el contenido en el archivo
archivo.puts "¡Hola mundo!"
archivo.puts "Este es un archivo de texto escrito en Ruby."

# Cierra el archivo
archivo.close()
```

El código anterior creará un archivo llamado "mi_archivo.txt" y escribirá dos líneas de texto dentro de él. Puedes abrir el archivo y ver el contenido para asegurarte de que se haya guardado correctamente.

## Profundizando

Ahora que sabes cómo escribir un archivo de texto, puedes explorar diferentes opciones para manipularlo y leerlo. Por ejemplo, puedes utilizar `File.read()` para leer el contenido completo del archivo o `File.readlines()` para leer línea por línea.

Además, puedes especificar diferentes modos de apertura del archivo, como "a" para añadir contenido a un archivo existente o "r+" para leer y escribir contenido en el archivo. También puedes utilizar la sintaxis de bloque `do` con `File.open()` para garantizar que el archivo se cierre automáticamente al finalizar el bloque.

## Ver también

- [Cómo leer un archivo de texto en Ruby](https://www.rubyguides.com/2015/05/reading-and-writing-files-in-ruby/)
- [Documentación oficial de Ruby sobre la clase File](https://ruby-doc.org/core-2.7.0/File.html)
- [Escribir y leer archivos de texto en Ruby por Avdi Grimm](https://www.rubyguides.com/2015/05/working-with-files-ruby/)