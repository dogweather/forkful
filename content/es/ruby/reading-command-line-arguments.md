---
title:    "Ruby: Leyendo argumentos de línea de comando"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## ¿Por qué leer argumentos de línea de comando?

Leer argumentos de línea de comando es una habilidad crucial para cualquier programador de Ruby. Con esta técnica, puedes mejorar la funcionalidad de tus programas al permitirte interactuar con ellos y pasarles información desde la línea de comando. También te permite crear programas más interactivos para tus usuarios. A continuación, te explicamos cómo hacerlo.

## Cómo hacerlo

Para leer argumentos de línea de comando en Ruby, utilizamos el método `ARGV`. Este método devuelve una matriz de todos los argumentos que se le pasan al programa desde la línea de comando. Veamos un ejemplo:

```Ruby
# Ejemplo de código para leer argumentos de línea de comando
nombre = ARGV[0]
puts "¡Hola #{nombre}!"
```

Si ejecutas este programa desde la línea de comando con `ruby programa.rb Juan`, la salida será `¡Hola Juan!`, ya que `Juan` será el primer argumento en la matriz `ARGV`.

También podemos acceder a argumentos específicos utilizando su índice en la matriz. Por ejemplo, si queremos imprimir el segundo argumento, utilizaríamos `puts ARGV[1]`. De esta manera, podemos acceder a todos los argumentos y utilizarlos en nuestro programa.

## Profundizando

Cuando utilizamos `ARGV`, los argumentos se almacenan como cadenas (strings) en la matriz. Esto significa que si queremos pasar un número como argumento, debemos convertirlo a un tipo de dato numérico antes de utilizarlo en nuestro programa. Podemos hacer esto usando el método `to_i` para convertir a un entero o `to_f` para convertir a un número decimal.

También es importante mencionar que los argumentos de línea de comando siempre se leen de izquierda a derecha y se deben separar por espacios. Por ejemplo, si queremos pasar varios nombres como argumentos, deberíamos escribirlos separados por espacios en la línea de comando.

¡Ahora estás listo para comenzar a leer argumentos de línea de comando en tus programas de Ruby!

## Ver también

- [Documentación de Ruby sobre ARGV](https://ruby-doc.org/core-2.7.1/ARGV.html)
- [Tutorial de Ruby sobre argumentos de línea de comando](https://www.rubyguides.com/2018/ argv-command-line-arguments/)