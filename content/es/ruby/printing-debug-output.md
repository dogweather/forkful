---
title:                "Imprimiendo salida de depuración"
html_title:           "Ruby: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

# ¿Qué y por qué?

El proceso de imprimir salidas de depuración (debug output) es una técnica comúnmente utilizada por los programadores para verificar el funcionamiento de su código y encontrar posibles errores. Al imprimir ciertas variables o mensajes específicos en diferentes puntos de un programa, los desarrolladores pueden tener una mejor comprensión de lo que está sucediendo en cada etapa y así detectar y solucionar problemas más fácilmente.

# Cómo hacerlo:

En Ruby, podemos imprimir salidas de depuración utilizando el método `puts` o `p`. A continuación se muestra un ejemplo de cómo imprimir un mensaje y el valor de una variable:

```
puts "¡Hola Ruby!"
p x
```

La salida sería:

```
¡Hola Ruby!
42
```

Aquí podemos ver cómo se imprime el mensaje y el valor de la variable `x`, que en este caso es igual a 42.

# Profundizando:

La impresión de salidas de depuración no es una técnica exclusiva de Ruby, de hecho, se utiliza en muchos otros lenguajes de programación. Sin embargo, Ruby cuenta con algunas características que la hacen especialmente adecuada para esta tarea.

Una alternativa común a la impresión de debug output es utilizar un depurador (debugger). Sin embargo, a diferencia de la impresión de salidas, los depuradores requieren de habilidades adicionales y pueden ser más complicados de usar.

En cuanto a la implementación, el método `puts` es más adecuado para imprimir mensajes y `p` para imprimir valores de variables. `p` también incluye información adicional como el tipo de dato de la variable, lo que puede ser útil para la depuración.

# Ver también:

- [Documentación oficial de Ruby sobre `puts`](https://ruby-doc.org/core-3.0.0/Kernel.html#method-i-puts)
- [Documentación oficial de Ruby sobre `p`](https://ruby-doc.org/core-3.0.0/Kernel.html#method-i-p)
- [Aprende a depurar en Ruby](https://www.rubyguides.com/2015/05/byebug/)