---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Clojure: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Convertir una cadena de texto a minúsculas es el proceso de transformar todas las letras en una cadena a su forma en minúscula. Los programadores hacen esto para asegurarse de que las comparaciones de cadenas sean precisas y para una mejor manipulación de datos.

## Cómo:
```Clojure
(.toLowerCase "HOLA MUNDO")
;=> "hola mundo"

(.toLowerCase "¡BIENVENIDO!")
;=> "¡bienvenido!"
```

## Profundizando:
- Contexto histórico: Convertir cadenas a minúsculas era una tarea tediosa en los primeros días de la programación, ya que se requería escribir una función específica para ese propósito cada vez que se necesitaba. Sin embargo, con el avance de los lenguajes de programación modernos, esta funcionalidad se ha vuelto mucho más sencilla de implementar.
- Alternativas: Algunos lenguajes de programación ofrecen funciones específicas para convertir cadenas a minúsculas, mientras que otros requieren que se utilicen métodos específicos de objetos de cadena para lograr el mismo resultado.
- Detalles de implementación: Convertir una cadena a minúsculas en Clojure es una tarea sencilla gracias a la función incorporada `toLowerCase`. Esta función utiliza la biblioteca Java `String` para realizar la conversión.

## Ver también:
- [Documentación oficial de Clojure sobre `toLowerCase`](https://clojuredocs.org/clojure.core/to-lower-case)
- [Otra forma de convertir cadenas a minúsculas en Clojure utilizando regex](https://stackoverflow.com/questions/9977880/how-to-convert-clojure-string-to-lower-case/26634953#26634953)