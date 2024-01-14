---
title:                "Elixir: Leyendo un archivo de texto"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un programador de Elixir, entonces probablemente ya estés familiarizado con la importancia de trabajar con archivos de texto en tus proyectos. Pero si eres nuevo en el lenguaje o solo estás buscando una forma más eficiente de manejar archivos de texto en Elixir, este artículo es para ti.

## Cómo hacerlo

Leer un archivo de texto es una tarea sencilla en Elixir gracias al módulo `File`. Primero, necesitas especificar la ruta del archivo y luego usar la función `read!` para obtener el contenido del mismo.

Por ejemplo, supongamos que tenemos un archivo llamado "ejemplo.txt" que contiene el siguiente texto:

```
¡Hola, mundo!
Este es un ejemplo de archivo de texto en Elixir.
```

Para leer este archivo en Elixir, haríamos lo siguiente:

```
contents = File.read!("ejemplo.txt")
```

Luego, si queremos imprimir el contenido del archivo en la consola, podríamos hacerlo de la siguiente manera:

```
IO.puts(contents)
```

El resultado en la consola sería:

```
¡Hola, mundo!
Este es un ejemplo de archivo de texto en Elixir.
```

## Profundizando

Aunque la función `read!` nos permite leer fácilmente el contenido de un archivo de texto, hay algunas cosas a tener en cuenta al trabajar con archivos en Elixir. Por ejemplo, si el archivo es muy grande, puede ser más eficiente usar la función `stream!` en lugar de `read!`, ya que esta última carga todo el contenido en memoria, mientras que `stream!` proporciona una forma de leer el archivo en fragmentos (o "streams").

También es importante tener en cuenta que diferentes sistemas operativos tienen diferentes convenciones de final de línea (EOL). En Elixir, podemos especificar la convención que queremos usar al leer o escribir en un archivo mediante el uso del parámetro `:eol` en nuestras funciones.

## Ver también

Si quieres aprender más sobre el manejo de archivos de texto en Elixir, puedes consultar estos recursos adicionales:

- [Documentación oficial sobre el módulo File en Elixir](https://hexdocs.pm/elixir/File.html)
- [Tutorial sobre el manejo de archivos en Elixir](https://www.freecodecamp.org/news/learn-elixir-by-creating-a-timezone-converter-622576de863b/)
- [Video tutorial sobre el manejo de archivos en Elixir](https://www.youtube.com/watch?v=97ciOT10S-8)