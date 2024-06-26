---
date: 2024-01-20 17:54:11.094975-07:00
description: "C\xF3mo Hacerlo: Leer un archivo de texto en Elixir es sencillo. Utiliza\
  \ `File.read/1` para leer el contenido completo o `File.stream!/3` para manejar\u2026"
lastmod: '2024-03-13T22:44:58.720380-06:00'
model: gpt-4-1106-preview
summary: Leer un archivo de texto en Elixir es sencillo.
title: Lectura de un archivo de texto
weight: 22
---

## Cómo Hacerlo:
Leer un archivo de texto en Elixir es sencillo. Utiliza `File.read/1` para leer el contenido completo o `File.stream!/3` para manejar archivos grandes línea por línea.

```elixir
# Leer contenido completo
{:ok, content} = File.read("mi_archivo.txt")
IO.puts(content)

# Manejo de archivos grandes
stream = File.stream!("mi_archivo_grande.txt")
Enum.each(stream, &IO.puts(&1))
```

Ejemplo de salida:

```
Hola, esto es una línea de texto.
Aquí hay otra línea de texto.
```

## Inmersión Profunda
Históricamente, Elixir hereda su enfoque en la manipulación de archivos de su lenguaje padre, Erlang. Alternativas a las funciones de archivo estándar incluyen bibliotecas como `CSV`, `Xlsxir` o `Poison` para JSON, que manejan formatos específicos. Internamente, `File.read` carga todo el archivo en la memoria, que es rápido para archivos pequeños. `File.stream!`, por otro lado, es perezoso (`lazy`), lo que significa que solo carga partes del archivo según sea necesario, ideal para archivos grandes o para el manejo de flujo de datos.

## Ver También
- [Documentación de Elixir para el módulo File](https://hexdocs.pm/elixir/File.html)
- [Elixir School: Manejo de archivos](https://elixirschool.com/es/lessons/basics/collections/#archivos)
- [Erlang's File Module para entender más sobre el backend de Elixir](http://erlang.org/doc/man/file.html)
