---
title:    "Ruby: Escribiendo un archivo de texto"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué escribir un archivo de texto

Escribir un archivo de texto en Ruby es una forma sencilla y eficiente de almacenar y organizar datos. Es especialmente útil cuando se trabaja con grandes cantidades de información que necesitan ser guardadas y consultadas en el futuro.

## Cómo hacerlo

Para escribir un archivo de texto en Ruby, se pueden seguir los siguientes pasos:

1. Abrir el archivo utilizando el método `File.open()` y especificando el nombre y la extensión del archivo a crear. También se puede incluir la opción `"w"` para indicar que se va a escribir en el archivo.

```Ruby
File.open("ejemplo.txt", "w") do |archivo|
  # código para escribir en el archivo
end
```

2. Una vez que el archivo está abierto, se pueden utilizar los métodos `puts` o `print` para escribir en él.

```Ruby
File.open("ejemplo.txt", "w") do |archivo|
  archivo.puts "Este es un ejemplo de texto" # escribe una línea en el archivo
  archivo.print "Esta es otra línea de texto" # escribe en la misma línea anterior
end
```

3. Finalmente, se debe cerrar el archivo utilizando el método `close`.

```Ruby
File.open("ejemplo.txt", "w") do |archivo|
  # código para escribir en el archivo
end # se cierra automáticamente al salir del bloque `do`
```

## Profundizando más

Además de escribir texto simple en un archivo, también es posible escribir variables y objetos utilizando los métodos `puts` o `print`. Estos métodos llaman automáticamente al método `to_s` de los objetos para convertirlos a cadenas de texto antes de escribirlos en el archivo.

También se pueden utilizar formatos de texto como `%`, `%s` o `%f` para imprimir valores numéricos o de cadena de forma más clara y ordenada.

## Ver también

- [Documentación oficial de Ruby: File class](https://ruby-doc.org/core-#{RUBY_VERSION}/File.html)
- [Escribiendo en archivos de texto con Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [Cómo utilizar formatos de texto en Ruby](https://mixandgo.com/learn/ruby-intermediate-tips-4-string-formatting)