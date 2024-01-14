---
title:                "Ruby: Escribiendo a la salida estándar"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

Escribir a la salida de error estándar en Ruby es una forma útil de depurar y manejar errores en nuestro código. Al utilizar esta técnica, podemos identificar y solucionar problemas en nuestras aplicaciones más rápidamente, lo que resulta en un código más robusto y eficiente.

## Cómo hacerlo

Para escribir a la salida de error estándar en Ruby, podemos usar el método `puts` junto con `$stderr`, que es la variable global que representa la salida de error estándar. Por ejemplo:

```Ruby
puts "Este es un mensaje de error" # se imprimirá en la salida de error estándar
```

Podemos incluso pasar una variable a `puts` para imprimir su valor en la salida de error estándar:

```Ruby
numero = 123
puts "El número es #{numero}" # se imprimirá en la salida de error estándar
```

El resultado en la terminal sería:

```
El número es 123
```

## Profundizando

Una forma común de utilizar la salida de error estándar en Ruby es en conjunción con `rescue` y `raise` para manejar excepciones. Por ejemplo, si queremos imprimir un mensaje de error personalizado cuando ocurre una excepción, podemos hacer lo siguiente:

```Ruby
begin
  # código que puede arrojar una excepción
rescue
  puts "Ocurrió un error: #{$!}" # $! representa el mensaje de error de la excepción
end
```

Esto resultará en un mensaje de error personalizado en la salida de error estándar cuando se capture una excepción.

## Ver también

- [Documentación de Ruby Error Handling](https://ruby-doc.org/core-2.7.1/Exception.html)
- [Artículo sobre manejo de errores en Ruby](https://www.rubyguides.com/2018/05/raise-catch/)
- [Preguntas frecuentes de Ruby sobre manejo de errores](https://ruby-doc.org/docs/keywords/1.9/Object.html#method-i-raise)

---

¡Esperamos que esta información te haya sido útil y que ahora te sientas más cómodo escribiendo a la salida de error estándar en Ruby! Recuerda siempre utilizar esta técnica para mejorar la calidad de tu código y ayudarte en la depuración de errores.

¡Hasta la próxima!