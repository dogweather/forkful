---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Leer un archivo de texto significa extraer los datos almacenados en él. Los programadores lo hacen frecuentemente para procesar información externa y para hacer sus aplicaciones más dinámicas y funcionales. 

## Cómo Hacerlo:

Para leer un archivo de texto en Elixir, puedes usar la función `File.read/1`. Aquí tienes el código básico.

```Elixir
{:ok, content} = File.read("your_file.txt")
IO.puts(content)
```

Esto leerá el archivo `your_file.txt` y imprimirá su contenido en la terminal. Si el archivo no se encuentra, la función dará un error.

## Profundización:

1. **Contexto histórico:** Elixir es un lenguaje de programación funcional moderno construido sobre la máquina virtual de Erlang. Esto lo hace particularmente adecuado para la programación concurrente y para tareas que requieren alta disponibilidad.
   
2. **Alternativas:** Otras funciones como `File.stream!/3` pueden ser útiles si estás trabajando con archivos grandes o si necesitas leer el archivo línea por línea.

```Elixir
File.stream!('your_file.txt')
|> Stream.map(&String.trim/1)
|> Enum.each(&IO.puts/1)
```
   
3. **Detalles de implementación:** `File.read/1` devuelve una tupla `{:ok, content}` en caso de éxito o `{:error, reason}` en caso de fallo. Es importante manejar estos casos en tu código para evitar fallos inesperados.

## Ver También:

- [Documentación Oficial de Elixir en File module](https://hexdocs.pm/elixir/File.html)
- [Sobre la lectura de archivos en Elixir](http://learningelixir.joekain.com/reading-files-in-elixir/)
- [Ejemplos de código de Elixir](https://elixir-examples.github.io/examples/read-a-file)
   
Recuerda que la práctica hace al maestro. ¡Sigue codificando!