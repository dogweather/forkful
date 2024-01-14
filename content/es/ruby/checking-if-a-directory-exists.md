---
title:    "Ruby: Comprobando si existe un directorio"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por qué

Si estás trabajando en un proyecto de programación en Ruby, es posible que te encuentres en la situación de necesitar verificar si un directorio existe o no. Esto puede ser útil para asegurarte de que un directorio necesario para tu código esté disponible o para crear un nuevo directorio en caso de que no exista. A continuación, te mostraré cómo hacerlo de manera sencilla.

## Cómo hacerlo

Para comprobar si un directorio existe en Ruby, podemos utilizar el método `Dir.exist?`. Este método acepta como argumento una ruta de directorio y devolverá `true` si el directorio existe y `false` si no existe. Veamos un ejemplo:

```Ruby
# Creamos una variable con la ruta de un directorio
directorio = "/home/user/Documentos/proyecto"

# Utilizamos el método Dir.exist? para comprobar si existe
if Dir.exist?(directorio)
  puts "El directorio existe"
else
  puts "El directorio no existe"
end
```

Si el directorio existe, el resultado de este código será "El directorio existe". De lo contrario, el resultado será "El directorio no existe". Fácil, ¿verdad? También podemos utilizar el método `File.exist?` para comprobar si un directorio existe, pero este método también puede utilizarse para comprobar la existencia de archivos, por lo que es menos específico.

## Profundizando

Si queremos obtener más información sobre el directorio que estamos comprobando, podemos utilizar el método `Dir.entries`. Este método devuelve un array con los nombres de los archivos y subdirectorios dentro del directorio especificado. Veamos un ejemplo:

```Ruby
# Creamos una variable con la ruta de un directorio
directorio = "/home/user/Documentos/proyecto"

# Utilizamos el método Dir.entries para obtener los archivos y subdirectorios
archivos = Dir.entries(directorio)

# Recorremos el array e imprimimos cada elemento
archivos.each do |archivo|
  puts archivo
end
```

El resultado de este código dependerá de los archivos y subdirectorios que haya dentro del directorio especificado. También podemos utilizar otros métodos, como `Dir.empty?` para comprobar si un directorio está vacío o `Dir.pwd` para obtener la ruta del directorio actual.

## Ver también
- [Documentación de Ruby sobre el método Dir.exist?](https://ruby-doc.org/core-2.7.1/Dir.html#method-c-exist-3F)
- [Tutorial de Codecademy sobre cómo manipular y crear directorios en Ruby](https://www.codecademy.com/learn/learn-ruby/modules/introduction-to-ruby-u/cheatsheet)
- [Video tutorial de Programación ATS sobre cómo verificar la existencia de archivos y directorios en Ruby](https://www.youtube.com/watch?v=1WUvR-e4cL8)