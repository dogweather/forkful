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

## ¿Qué y por qué?

Leer un archivo de texto simplemente significa que un programa está accediendo a un archivo que contiene información en formato de texto. Los programadores a menudo hacen esto para leer y manipular datos externos en sus programas, como configuraciones, bases de datos o registros de usuarios.

## ¿Cómo hacerlo?

```ruby
# Este código abre un archivo llamado "ejemplo.txt" y lee su contenido línea por línea.
File.open("ejemplo.txt", "r") do |archivo|
  archivo.each_line do |linea|
    puts linea
  end
end
```

Entrada del archivo "ejemplo.txt":
```
Hola, soy un archivo de texto.
Esta es otra línea.
Y esto es una tercera línea.
```

Salida del código:
```
Hola, soy un archivo de texto.
Esta es otra línea.
Y esto es una tercera línea.
```

## Inmersión profunda

Leer archivos de texto ha sido una técnica común en la programación desde los primeros días de las computadoras. Alternativas a la lectura de archivos de texto incluyen el uso de una base de datos o aplicar técnicas de web scraping para obtener datos de páginas web.

En Ruby, la función `File.open()` se puede usar para abrir un archivo en modo de lectura (`"r"`) o escritura (`"w"`). También se puede especificar una codificación para archivos que contienen caracteres especiales.

## Véase también

- [Documentación oficial de la clase `File`](https://ruby-doc.org/core-3.0.2/File.html)
- [Otros métodos para manipular archivos en Ruby](https://www.rubyguides.com/ruby-file/)