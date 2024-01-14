---
title:                "Ruby: Leyendo un archivo de texto"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Muchas veces en la programación, necesitamos leer archivos de texto para manipular datos o realizar operaciones específicas. Aprender cómo leer y manipular estos archivos puede ser una habilidad muy útil en el desarrollo de aplicaciones y scripts con Ruby.

## Como hacerlo

Para leer un archivo de texto en Ruby, primero debemos abrirlo usando el método `File.open()`. Dentro de los paréntesis, indicamos el nombre del archivo y el modo de lectura que deseamos utilizar. Por ejemplo, si queremos leer un archivo llamado "datos.txt", podríamos usar el siguiente código:

```Ruby
archivo = File.open("datos.txt", "r")
```

El modo de lectura "r" nos permite leer el archivo sin modificarlo. Ahora que tenemos el archivo abierto, podemos comenzar a manipularlo. Para leer el contenido del archivo línea por línea, podemos utilizar el método `each_line` de la siguiente manera:

```Ruby
archivo.each_line do |linea|
  puts linea
end
```

Este código imprimirá cada línea del archivo en la consola. También podemos guardar el contenido del archivo en una variable y manipularlo como un arreglo:

```Ruby
contenido = archivo.readlines
```

Ahora podemos usar métodos como `split` o `gsub` para separar o reemplazar el contenido de cada línea según sea necesario.

Al finalizar, siempre debemos cerrar el archivo usando el método `close` para liberar los recursos del sistema:

```Ruby
archivo.close
```

## Profundizando

Ruby ofrece una gran variedad de métodos para leer y manipular archivos de texto. Por ejemplo, podemos usar métodos como `read`, `getc` o `readchar` para leer el archivo byte por byte. También podemos elegir el modo de lectura "w" para abrir un archivo en modo escritura y sobrescribir su contenido.

Además, Ruby nos permite leer archivos comprimidos usando la gema `zlib`, y archivos CSV usando la gema `csv`.

## Ver también

- Documentación oficial de Ruby sobre lectura de archivos: https://ruby-doc.org/core-2.7.1/File.html
- Gema zlib: https://rubygems.org/gems/zlib/versions/0.2.11
- Gema CSV: https://rubygems.org/gems/csv/versions/3.1.9