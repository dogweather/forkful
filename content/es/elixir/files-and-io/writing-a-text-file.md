---
aliases:
- /es/elixir/writing-a-text-file/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:32.030416-07:00
description: "Escribir en un archivo de texto en Elixir es una habilidad esencial\
  \ para los desarrolladores, permitiendo la persistencia de datos, registro o exportaci\xF3\
  n\u2026"
lastmod: 2024-02-18 23:09:09.676864
model: gpt-4-0125-preview
summary: "Escribir en un archivo de texto en Elixir es una habilidad esencial para\
  \ los desarrolladores, permitiendo la persistencia de datos, registro o exportaci\xF3\
  n\u2026"
title: Escribiendo un archivo de texto
---

{{< edit_this_page >}}

## Qué y Por Qué?

Escribir en un archivo de texto en Elixir es una habilidad esencial para los desarrolladores, permitiendo la persistencia de datos, registro o exportación de contenido legible por humanos. Los programadores logran esto para guardar el estado de la aplicación, información para depurar, configuraciones o cualquier intercambio de datos entre sistemas que prefieren un formato ubicuo como el texto.

## Cómo hacerlo:

Elixir facilita el manejo de archivos con módulos incorporados. La forma principal de escribir en un archivo es usando las funciones `File.write/2` o `File.write!/2`, donde la primera devuelve una tupla `:ok` o `:error` y la segunda genera un error en caso de fallo.

He aquí un ejemplo simple:

```elixir
# Escribiendo en un archivo, mensaje simple
File.write("hello.txt", "¡Hola, Mundo!")

# Cuando ejecutas el código, se crea 'hello.txt' con "¡Hola, Mundo!" como contenido
```

Para agregar contenido a archivos, usarías `File.open/3` con las opciones `[:write, :append]`, luego `IO.binwrite/2` para añadir el contenido:

```elixir
# Agregando a un archivo
{:ok, archivo} = File.open("hello.txt", [:write, :append])
IO.binwrite(archivo, "\nAgreguemos otra línea.")
File.close(archivo)

# Ahora 'hello.txt' incluye una segunda línea "Agreguemos otra línea."
```

Si trabajas con grandes datos o necesitas más control sobre el proceso de escritura, podrías usar el módulo `Stream` para escribir datos en el archivo de forma perezosa:

```elixir
# Escribiendo un gran conjunto de datos de forma perezosa
datos_stream = Stream.iterate(0, &(&1 + 1))
            |> Stream.map(&("Número: #{&1}\n"))
            |> Stream.take(10)

File.open!("numbers.txt", [:write], fn archivo ->
  Enum.each(datos_stream, fn línea ->
    IO.write(archivo, línea)
  end)
end)

# Esto crea 'numbers.txt', escribiendo números del 0 al 9, cada uno en una nueva línea.
```

Para proyectos que requieren un manejo de archivos más sofisticado, podrías investigar bibliotecas de terceros como `CSV`, que ofrece funcionalidades a medida para la manipulación de archivos CSV pero recuerda, para muchos propósitos, las capacidades incorporadas en Elixir son más que suficientes.
