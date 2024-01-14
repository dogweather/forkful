---
title:    "Ruby: Leyendo argumentos de línea de comandos."
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué leer argumentos de línea de comandos en Ruby

Al escribir cualquier tipo de programa, es importante tener en cuenta la posibilidad de que el usuario desee proporcionar información al ejecutarlo. Los argumentos de línea de comandos son una forma útil de permitir que el usuario personalice la ejecución de un programa. A continuación, explicamos cómo implementarlo en Ruby.

## Cómo hacerlo

Para leer argumentos de línea de comandos en Ruby, utilizaremos la variable especial `ARGV`. Esta variable contiene un array con todos los argumentos proporcionados por el usuario al ejecutar el programa. Por ejemplo, si ejecutamos desde la terminal el comando `ruby programa.rb hola adiós`, la variable `ARGV` contendrá `["hola", "adiós"]`.

Para acceder a los argumentos individuales, podemos usar el indexado del array (`ARGV[0]` para el primer argumento, `ARGV[1]` para el segundo, etc) o el método `fetch()` para manejar errores en caso de que el usuario no haya proporcionado suficientes argumentos.

```ruby
# programa.rb

# Obtener el primer argumento
primer_argumento = ARGV[0]

# Obtener el segundo argumento con fetch()
segundo_argumento = ARGV.fetch(1, 'default')

# Imprimir los argumentos y su cantidad
puts "Los argumentos proporcionados son: #{primer_argumento} y #{segundo_argumento}"
puts "Cantidad de argumentos: #{ARGV.length}"
```

Al ejecutar este programa con `ruby programa.rb hola adiós`, el output sería:

```bash
Los argumentos proporcionados son: hola y adiós
Cantidad de argumentos: 2
```

## Profundizando

Además de acceder a los argumentos de línea de comandos, también podemos validar su formato y realizar acciones en base a ellos. Por ejemplo, podríamos utilizar un bucle `each` para iterar sobre todos los argumentos y verificar si son números enteros, como en el siguiente ejemplo:

```ruby
# programa.rb

# Método para verificar si un string es un número entero
def es_entero?(string)
  string.to_i.to_s == string
end

# Verificar si todos los argumentos son enteros
ARGV.each do |argumento|
  if es_entero?(argumento)
    puts "#{argumento} es un número entero"
  else
    puts "#{argumento} no es un número entero"
  end
end
```

Al ejecutar este programa con `ruby programa.rb 1 2 hola`, el output sería:

```bash
1 es un número entero
2 es un número entero
hola no es un número entero
```

Con esta información, podemos personalizar aún más la ejecución de nuestros programas y hacerlos más interactivos y útiles para el usuario.

## Ver también

- [Documentación oficial de Ruby sobre argumentos de línea de comandos](https://ruby-doc.org/docs/CommandLineArguments.html)
- [Blog post (en inglés) sobre argumentos de línea de comandos en Ruby](https://www.rubyguides.com/2012/02/ruby-command-line-args/)