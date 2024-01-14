---
title:    "Ruby: Escribiendo a la salida de error estándar"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué escribir en el error estándar

Escribir en el error estándar es una parte fundamental de la programación en Ruby. Al imprimir mensajes en el error estándar, podemos obtener información sobre los posibles errores en nuestro código y así poder corregirlos de manera efectiva.

## Cómo hacerlo

Para escribir en el error estándar en Ruby, utilizamos el método `warn`. Este método toma una cadena como argumento y la imprime en el error estándar. Veamos un ejemplo:

```Ruby
def dividir(a, b)
    if b == 0
        warn "No se puede dividir entre 0"
        return nil
    else
        return a / b
    end
end

puts dividir(8, 2)
# Output => 4

puts dividir(5, 0)
# Output => nil
# "No se puede dividir entre 0" se imprimirá en el error estándar
```

En el ejemplo anterior, estamos dividiendo dos números y en caso de que el segundo sea 0, imprimimos un mensaje de error en el error estándar.

También podemos utilizar el método `raise` para imprimir un mensaje de error en el error estándar y detener la ejecución del programa:

```Ruby
def encontrar_indice(arr, num)
    if arr.include? num
        return arr.index(num)
    else
        warn "#{num} no se encuentra en el arreglo"
        # Imprimimos el mensaje de error en el error estándar
        raise "Elemento no encontrado en el array"
        # Levantamos una excepción e interrumpimos la ejecución del programa
    end
end

numeros = [1, 2, 3, 4, 5]

puts encontrar_indice(numeros, 6)
# Output => nil
# "6 no se encuentra en el arreglo" se imprimirá en el error estándar
# Y el programa se detendrá al arrojar la excepción
```

## Profundizando

Ahora que sabemos cómo imprimir en el error estándar en Ruby, es importante tener en cuenta que también podemos redirigir el error estándar a un archivo de texto. Esto puede ser útil en caso de que necesitemos guardar los mensajes de error para futuras referencias o análisis.

Para hacer esto, utilizamos el operador `>` seguido del nombre del archivo en el que queremos guardar los mensajes de error:

```Ruby
10 / 0 > "error.txt"
# Se escribirá "división entre 0" en el archivo "error.txt"
```

También podemos utilizar el operador `>>` para añadir los mensajes de error a un archivo existente en lugar de sobrescribirlo:

```Ruby
5 / 0 >> "errores.log"
# Se añadirá "división entre 0" al final del archivo "errores.log"
```

## Ver también

- [Documentación de Ruby sobre el método `warn`](https://ruby-doc.org/core-3.0.1/Kernel.html#method-i-warn)
- [Documentación de Ruby sobre el método `raise`](https://ruby-doc.org/core-3.0.1/Kernel.html#method-i-raise)
- [Tutorial de Ruby: Manejo de excepciones](https://www.tutorialspoint.com/ruby/ruby_exceptions.htm)