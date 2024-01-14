---
title:                "Clojure: Utilizando expresiones regulares"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué

Las expresiones regulares son una herramienta poderosa en la programación, ya que permiten realizar búsquedas y manipulaciones de texto de manera eficiente. Son especialmente útiles para realizar tareas como validación de formularios, análisis de datos y extracción de información específica de un texto.

## Cómo hacerlo

Para utilizar expresiones regulares en Clojure, primero debemos importar el módulo ```clojure.string```. Luego, podemos utilizar la función ```re-find``` para buscar patrones en una cadena de texto. Por ejemplo, si queremos encontrar todas las palabras que empiecen con la letra "c" en una oración, podemos hacer lo siguiente:

```Clojure
(require '[clojure.string :as str])

(def oracion "Clojure es un lenguaje de programación funcional")
(re-find #"c\S+" oracion)
```

Esto nos devolverá una lista con todas las palabras que comienzan con "c" en la oración, en este caso ```["Clojure" "c" "comienzan" "con" "com" "ción" "cional"]```. 
En el código anterior, utilizamos la expresión regular ```#"c\S+"```, donde la letra "c" indica el inicio de la palabra y el meta-carácter ```\S+``` representa cualquier cantidad de caracteres no espacios en blanco.

Otra función útil para trabajar con expresiones regulares en Clojure es ```re-seq```, que nos permite obtener todas las coincidencias en vez de solo la primera. Por ejemplo, si queremos encontrar todos los números en una cadena de texto, podemos utilizar la expresión regular ```#"(\d+)"``` y la función ```re-seq```, como se muestra a continuación:

```Clojure
(def cadena "Hoy es 15 de noviembre de 2021")
(re-seq #"\d+" cadena)
```

Esto nos devolverá una lista con todos los números encontrados en la cadena, en este caso ```["15" "2021"]```. 
Con estas funciones y expresiones regulares, podemos realizar una gran cantidad de operaciones de búsqueda y manipulación de texto de manera eficiente en Clojure.

## Profundizando

Además de las funciones mencionadas anteriormente, Clojure también ofrece otras herramientas para trabajar con expresiones regulares. Por ejemplo, ```re-pattern``` nos permite compilar una expresión regular antes de utilizarla, lo cual puede mejorar la eficiencia en ciertos casos. Asimismo, podemos utilizar ```re-find-all``` para obtener todas las coincidencias en un texto y ```re-groups``` para obtener grupos específicos dentro de una coincidencia.

Es importante tener en cuenta que las expresiones regulares pueden ser un poco complicadas de entender al comienzo, pero con la práctica se vuelven una herramienta muy útil y poderosa en la programación. Se recomienda utilizar recursos en línea para aprender más detalles sobre su uso en Clojure y practicar con diferentes patrones y funciones.

## Ver también

- [Documentación oficial de funciones de expresiones regulares en Clojure](https://clojuredocs.org/clojure.string)
- [Artículo de introducción a expresiones regulares en Clojure](https://www.braveclojure.com/regular-expressions/)

¡Ahora estás listo para utilizar expresiones regulares en tus proyectos de Clojure! Esperamos que esta guía haya sido útil para comprender el uso y la importancia de esta herramienta en la programación. ¡A seguir aprendiendo y mejorando tus habilidades en Clojure!