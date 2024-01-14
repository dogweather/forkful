---
title:    "Ruby: Eliminando caracteres que coinciden con un patrón"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## ¿Por qué borrar caracteres que coinciden con un patrón en Ruby?

En la programación, a menudo nos encontramos con situaciones en las que necesitamos manipular cadenas de texto para eliminar ciertos caracteres que coinciden con un patrón específico. Por ejemplo, puede ser necesario eliminar todas las vocales de una cadena de texto o todas las letras mayúsculas. En Ruby, hay varias formas de lograr esto, y en este artículo te mostraremos cómo hacerlo.

## Cómo realizar este proceso en Ruby

La forma más básica de borrar caracteres que coinciden con un patrón en Ruby es utilizando el método `gsub`. Este método toma dos argumentos: el patrón que queremos eliminar y el texto en el que queremos buscar y eliminar ese patrón. Por ejemplo, si queremos eliminar todas las vocales de una cadena de texto, podemos hacerlo de la siguiente manera:

```Ruby
texto = "Hola mundo!"

texto.gsub(/[aeiou]/,'')
# Salida: Hl mnd!
```

En este ejemplo, utilizamos una expresión regular para indicar que queremos eliminar todas las vocales (representadas por las letras entre corchetes) de la cadena de texto. El segundo argumento es una cadena vacía, lo que significa que queremos reemplazar las vocales con nada, es decir, eliminarlas.

También es posible utilizar el método `delete` en lugar de `gsub` para eliminar caracteres. Este método toma cualquier número de argumentos y elimina todos los caracteres que coincidan con esos argumentos. Por ejemplo, si queremos eliminar todas las vocales y las letras mayúsculas de la cadena de texto anterior, podemos hacerlo de la siguiente manera:

```Ruby
texto = "Hola mundo!"

texto.delete('aeiou', 'A-Z')
# Salida: vlnd!
```

En este ejemplo, hemos utilizado dos argumentos en el método `delete`: `aeiou` para eliminar todas las vocales y `A-Z` para eliminar todas las letras mayúsculas. En ambos casos, el resultado es el mismo, pero el método `delete` ofrece una forma más concisa de escribirlo.

## Profundizando en el proceso

Si queremos ser más específicos con nuestro patrón, podemos utilizar expresiones regulares más complejas para eliminar caracteres que coincidan con un cierto criterio. Por ejemplo, si queremos eliminar todos los caracteres que no sean letras de la cadena de texto, podemos usar la siguiente expresión regular:

```Ruby
texto = "¡123 Hola mundo!@#"

texto.gsub(/[^a-zA-Z]/, '')
# Salida: Holamundo
```

En este caso, utilizamos el corchete de negación `[^]` para indicar que queremos eliminar todos los caracteres que no sean letras. También podemos agregar otros caracteres a esa expresión regular, como por ejemplo los números, para eliminarlos también.

## Ver también

- Documentación de `gsub`: https://ruby-doc.org/core-2.7.3/String.html#method-i-gsub
- Documentación de `delete`: https://ruby-doc.org/core-2.7.3/String.html#method-i-delete
- Tutorial de expresiones regulares en Ruby: https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm