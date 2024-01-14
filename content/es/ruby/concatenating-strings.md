---
title:    "Ruby: Uniendo cadenas"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por qué

La concatenación de cadenas es una habilidad esencial en programación, especialmente en Ruby. Permite combinar varias cadenas en una sola, lo que es útil para crear mensajes personalizados y mostrar datos en un formato fácil de entender. Además, ayuda a optimizar el rendimiento del código y facilita la escritura de funciones complejas.

## Cómo hacerlo

Podemos concatenar cadenas en Ruby de varias formas, la más común es utilizando el operador `+` o el método `concat`. Veamos un ejemplo:

```Ruby
nombre = "Juan"
apellido = "Pérez"

# Utilizando el operador +
nombre_completo = nombre + " " + apellido 

# Utilizando el método concat
nombre_completo = nombre.concat(" ", apellido)

puts nombre_completo
# Output: Juan Pérez
```

Ambos métodos producen el mismo resultado, una cadena con el nombre y apellido combinados. Sin embargo, es importante tener en cuenta que utilizando `+` se crea una cadena completamente nueva, mientras que `concat` modifica la cadena original.

También podemos utilizar la interpolación de cadenas, que nos permite insertar valores de variables en una cadena. Veamos un ejemplo:

```Ruby
edad = 30
mensaje = "Tengo #{edad} años"
puts mensaje
# Output: Tengo 30 años
```

Otra forma de concatenar cadenas es utilizando el método `join` en una array de cadenas. Este método une todas las cadenas de la array en una sola. Veamos un ejemplo:

```Ruby
nombres = ["María", "José", "Luis"]
nombres_completos = nombres.join(" ")
puts nombres_completos
# Output: María José Luis
```

## Profundizando

En Ruby, las cadenas son objetos mutables, lo que significa que pueden ser modificadas. Por lo tanto, es posible concatenar cadenas de manera más eficiente utilizando el método `<<`, que agrega una cadena al final de otra.

```Ruby
seccion1 = "¡Hola "
seccion2 = "mundo!"
seccion1 << seccion2
puts seccion1
# Output: ¡Hola mundo!
```

También podemos encadenar varios métodos de concatenación en una sola línea, lo que nos permite crear cadenas más complejas. Por ejemplo:

```Ruby
nombre = "Juan"
apellido = "Pérez"
email = "juanperez@example.com"

mensaje = "Bienvenido, #{nombre.concat(" ", apellido)}. Tu correo es: #{email}"
puts mensaje
# Output: Bienvenido, Juan Pérez. Tu correo es: juanperez@example.com
```

## Ver también

- [Documentación oficial de Ruby sobre la concatenación de cadenas](https://ruby-doc.org/core-3.0.0/String.html#method-i-2B)
- [Tutorial de Ruby sobre la concatenación de cadenas](https://www.rubyguides.com/2020/01/ruby-string-concatenation/)
- [Ejercicios prácticos de concatenación de cadenas en Ruby](https://www.codewars.com/kata/54a91a4883a7de5d7800009c)