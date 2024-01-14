---
title:    "Ruby: Extrayendo subcadenas"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

##Por qué

Extraer subcadenas es una tarea común en la programación Ruby. Esta función es útil cuando se desea obtener una parte específica de una cadena, por ejemplo, si se quiere extraer solo el nombre del dominio de una dirección de correo electrónico completa.

##Cómo hacerlo

Para extraer una subcadena en Ruby, se utiliza el método `[]` seguido de los índices de inicio y fin entre corchetes. Por ejemplo, si queremos extraer los primeros tres caracteres de una cadena, usaríamos el siguiente código:

```Ruby
cadena = "Hola mundo"

puts cadena[0,3]
```

El resultado sería `Hol`, ya que el primer índice es el 0 y el segundo es el 3, por lo tanto, se extraen los caracteres en ese rango.

También se pueden utilizar números negativos para indicar el conteo desde el final de la cadena. Por ejemplo, si queremos extraer los últimos dos caracteres de una cadena, usaríamos:

```Ruby
cadena = "Ruby rocks!"

puts cadena[-2,2]
```

El resultado sería `ks`, ya que se toman los dos últimos caracteres.

Otra forma de extraer subcadenas es utilizando el método `slice()` y pasando como argumentos los índices de inicio y fin. Este método es equivalente al método `[]` y se puede utilizar de la siguiente manera:

```Ruby
cadena = "Programación Ruby"

puts cadena.slice(0,11)
```

El resultado sería `Programación`, ya que se toman los primeros once caracteres.

##Profundizando

La función de extraer subcadenas también permite utilizar expresiones regulares para ser más específicos en la selección de los caracteres a extraer. Por ejemplo, si queremos extraer solo los números de una cadena, podemos usar lo siguiente:

```Ruby
cadena = "abc123def"

puts cadena[/\d+/]
```

El resultado sería `123`, ya que la expresión regular `\d+` indica que seleccione uno o más dígitos.

También se pueden utilizar los métodos `match()` y `scan()` para extraer subcadenas basados en expresiones regulares. Estos métodos ofrecen mayor flexibilidad y control sobre la extracción de los datos.

##Ver también

- [Documentación oficial de Ruby sobre la función de extraer subcadenas](https://ruby-doc.org/core-2.6/String.html#method-i-slice)
- [Ejemplos prácticos de extracción de subcadenas en Ruby](https://www.geeksforgeeks.org/ruby-regexp-scan-function/)
- [Tutorial en español sobre expresiones regulares en Ruby](https://es.wikibooks.org/wiki/Programaci%C3%B3n_en_Ruby/Expresiones_regulares)