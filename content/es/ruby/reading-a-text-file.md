---
title:                "Leyendo un archivo de texto"
html_title:           "Ruby: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un entusiasta de la programación o simplemente estás aprendiendo a codificar, es probable que en algún momento necesites leer un archivo de texto en Ruby. Ya sea para analizar datos, manipular información o simplemente para entender mejor un programa, saber cómo leer un archivo de texto en Ruby es una habilidad valiosa a tener.

## Cómo hacerlo

```Ruby
# Primero, abrimos el archivo usando la función File.open y proporcionando el nombre del archivo y el modo de apertura (en este caso, solo lectura)
archivo = File.open("texto.txt", "r")

# Ahora podemos leer el contenido del archivo y almacenarlo en una variable
contenido = archivo.read

# Si queremos imprimir el contenido en la consola, podemos hacerlo de la siguiente manera
puts contenido

# También podemos usar un bucle para leer el archivo línea por línea
archivo.each do |linea|
    puts linea
end

# No olvides cerrar el archivo para liberar recursos
archivo.close
```

Al ejecutar este código, podrás ver el contenido del archivo impreso en la consola o terminal. Ten en cuenta que si el archivo es muy grande, puede ser más eficiente utilizar un bucle para leerlo en lugar de almacenar todo el contenido en una sola variable.

## Profundizando

Hay muchas formas de leer un archivo de texto en Ruby, y una de ellas es utilizando los métodos de la clase File. Por ejemplo, además de usar `read` y `each`, también podemos usar `readlines` para obtener un arreglo con todas las líneas del archivo. Además, podemos especificar una línea de inicio y una línea final para leer solo una parte del archivo.

Otra opción es usar la gema (gem) "CSV" para leer archivos de texto en formato CSV (valores separados por comas). Esta gema proporciona métodos específicos para trabajar con este tipo de archivos y es especialmente útil si necesitas analizar datos o manipularlos en un programa.

## Ver también

- [Documentación oficial de Ruby sobre lectura de archivos](https://ruby-doc.org/core-3.0.1/File.html)
- [Gema CSV en RubyGems.org](https://rubygems.org/gems/csv)
- [Tutorial en español sobre manipulación de archivos en Ruby](https://rubeus.net/mastering-ruby-fundamentals/array-manipulation.html)