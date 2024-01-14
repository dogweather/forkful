---
title:    "Ruby: Extracción de subcadenas"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué
Uno de los conceptos básicos de la programación es la manipulación de cadenas de texto. Extraer subcadenas de una cadena más grande puede ser muy útil al trabajar con datos específicos o al crear un programa con una función específica.

## Cómo hacerlo
La extracción de subcadenas en Ruby se realiza utilizando el método `slice` o su alias `[]`. Este método toma dos argumentos: el índice de inicio y la longitud de la subcadena deseada. Por ejemplo, si tenemos la siguiente cadena: 

```Ruby
cadena = "¡Hola Mundo!"
```

Y queremos extraer solo la palabra "Hola", podemos hacerlo de la siguiente manera:

```Ruby
cadena[1,4] # => "Hola"
```

También podemos utilizar números negativos para referirnos a la cadena desde el final. Por ejemplo, si queremos extraer "Mundo" podríamos hacerlo así:

```Ruby
cadena[-6,5] # => "Mundo"
```

La extracción de subcadenas también se puede realizar utilizando rangos. Por ejemplo, si queremos extraer "Hola Mundo" podríamos hacerlo de la siguiente manera:

```Ruby
cadena[1..9] # => "Hola Mundo"
```

## Profundizando más
Además de las formas básicas de extracción de subcadenas, Ruby también ofrece una serie de métodos útiles para trabajar con cadenas de texto. Por ejemplo, el método `start_with?` nos permite verificar si una cadena comienza con una subcadena específica:

```Ruby
cadena = "¡Hola!"
cadena.start_with?("¡") # => true
```

También podemos utilizar el método `include?` para comprobar si una cadena contiene una subcadena específica:

```Ruby
cadena = "¡Hola Mundo!"
cadena.include?("Hola") # => true
```

## Ver también
- [Ruby Docs: Strings](https://ruby-doc.org/core-2.7.1/String.html)