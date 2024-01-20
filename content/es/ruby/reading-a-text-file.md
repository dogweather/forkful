---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Leer un archivo de texto es extraer y procesar la información contenida en dicho archivo. Los programadores hacen esto para trabajar con datos externos.

## ¿Cómo hacerlo?

Aquí te proporciono un ejemplo detallado. Primero, debes tener un archivo de texto, digamos que se llama `prueba.txt`.

```Ruby
# Ruby 3.0.0
filename = 'prueba.txt'

puts "El archivo contiene lo siguiente:"
puts File.read(filename)
```

Si `prueba.txt` contiene "¡Hola, mundo!", verás lo siguiente:

```Ruby
El archivo contiene lo siguiente:
¡Hola, mundo!
```

## Análisis en Profundidad

1. Contexto histórico: la capacidad de leer archivos de texto ha existido desde los primeros días de la programación y sigue siendo una habilidad relevante en la actualidad.
   
2. Alternativas: En lugar de `File.read`, podrías usar `IO.foreach`. Esta es una excelente opción si estás trabajando con archivos grandes que te gustaría leer línea por línea.

```Ruby
# Ruby 3.0.0
filename = 'prueba.txt'

IO.foreach(filename) do |line|
  puts line
end
```

3. Detalles de la implementación: `File.read` abre el archivo, lee su contenido, lo cierra y devuelve el contenido como una cadena.

## Enlaces Relacionados

1. Documentación oficial de Ruby para la clase File: [https://ruby-doc.org/core-3.0.0/File.html](https://ruby-doc.org/core-3.0.0/File.html)
2. Documentación oficial de Ruby para la clase IO: [https://ruby-doc.org/core-3.0.0/IO.html](https://ruby-doc.org/core-3.0.0/IO.html)