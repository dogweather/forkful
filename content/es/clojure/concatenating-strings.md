---
title:    "Clojure: Concatenando cadenas"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

La concatenación de cadenas es una técnica comúnmente utilizada en programación para combinar dos o más cadenas de texto en una sola. Esto puede ser útil en situaciones en las que se desea mostrar información dinámica en una interfaz de usuario o generar mensajes personalizados. Aprender a concatenar cadenas en Clojure te permitirá crear programas más dinámicos y flexibles.

## Cómo hacerlo

Para concatenar cadenas en Clojure, puedes utilizar la función "str", que toma cualquier número de argumentos de texto y los une en una sola cadena. Por ejemplo:

```Clojure
(str "¡Hola" ", " "mundo" "!") ; imprimiría "¡Hola, mundo!"
```

También puedes utilizar la función "format" para concatenar cadenas en un formato específico. Esta función toma una cadena de formato y cualquier número de argumentos y los combina en una sola cadena formateada. Por ejemplo:

```Clojure
(format "Hoy es %s, el %s de %s" "martes" "5" "enero") ; imprimiría "Hoy es martes, el 5 de enero"
```

## Profundizando

Es importante tener en cuenta que la concatenación de cadenas en Clojure es un proceso inmutable, lo que significa que no altera las cadenas originales sino que crea una nueva cadena combinando las existentes. Por lo tanto, es esencial utilizar la función "str" o "format" para asignar el resultado a una variable o imprimirlo.

Además, es importante tener en cuenta el rendimiento al concatenar cadenas en grandes cantidades. Debido a que Clojure es un lenguaje funcional, cada vez que se realiza una concatenación de cadenas, se crea una nueva cadena y se descarta la anterior. Por lo tanto, en situaciones en las que se necesitan concatenar grandes cantidades de cadenas, se recomienda utilizar conjuntos o vectores y luego unirlos utilizando "apply" para mejorar el rendimiento.

## Ver también

- [Documentación oficial de Clojure sobre la concatenación de cadenas](https://clojure.org/guides/strings)
- [Ejemplos de concatenación de cadenas en Clojure](https://www.javaer101.com/es/article/4743409.html)
- [Comparación de rendimiento de diferentes métodos de concatenación en Clojure](https://medium.com/@maximshvimmanuel/converting-string-to-edn-in-a-performance-oriented-clojure-eb3b7f47d0d5) (en inglés)