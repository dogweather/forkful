---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Kotlin: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
La conversión de una cadena de texto a minúsculas es una técnica común utilizada por los programadores para asegurar que los datos sean uniformes y fáciles de manipular. Al convertir una cadena de texto a minúsculas, se elimina la variabilidad en la capitalización de las letras, lo que facilita la búsqueda y comparación de datos.

## Cómo hacerlo:
```Kotlin
val str = "Hola Mundo"
println(str.toLowerCase())
// output: hola mundo
```
En el ejemplo anterior, utilizamos el método `toLowerCase()` para convertir la cadena de texto "Hola Mundo" a minúsculas y luego lo imprimimos en la consola. 

También podemos utilizar este método para comparar dos cadenas de texto sin importar su capitalización:
```Kotlin
val str1 = "Hola"
val str2 = "hOlA"
println(str1.toLowerCase() == str2.toLowerCase())
// output: true
```

## Profundizando:
La conversión de cadenas de texto a minúsculas ha sido una práctica común desde los comienzos de la programación. Antes de la aparición de los lenguajes de programación modernos, los programadores tenían que utilizar técnicas más complejas para lograr este resultado, como el mapeo de caracteres de mayúsculas a minúsculas.

Otra forma de convertir una cadena de texto a minúsculas en Kotlin es utilizando el método `replace()` y una expresión regular para reemplazar todas las letras mayúsculas por minúsculas. Este método puede ser útil si se desea personalizar la conversión y no solo convertir todas las letras a minúsculas.

En cuanto a la implementación, el método `toLowerCase()` simplemente llama al método `toLowerCase()` de la clase `java.lang.String`, ya que en Kotlin, una cadena de texto es en realidad una instancia de dicha clase.

## Ver también:
- [Documentación oficial de Kotlin sobre el método`toLowerCase()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)
- [Expresiones regulares en Kotlin](https://kotlinlang.org/docs/reference/regexp.html)
- [Cadenas de texto en Kotlin](https://kotlinlang.org/docs/reference/basic-types.html#strings)