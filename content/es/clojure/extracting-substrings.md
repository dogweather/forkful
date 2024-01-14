---
title:    "Clojure: Extrayendo subcadenas"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por Qué

Extraer subcadenas es una práctica común y útil en la programación. Puede ser útil para manipular y analizar cadenas de texto, especialmente en aplicaciones de procesamiento de lenguaje natural donde la información relevante se encuentra en ciertas partes de una cadena más larga.

## Cómo Hacerlo

Para extraer una subcadena en Clojure, podemos utilizar la función `subs`. Aquí hay un ejemplo de cómo utilizarla para obtener sólo los primeros tres caracteres de una cadena:

```
Clojure (subs "Hola Mundo" 0 3)
"Hol"
```

En este ejemplo, `subs` toma tres argumentos: la cadena original, el índice inicial a partir del cual extraer y el índice final antes del cual detenerse. En este caso, queremos iniciar desde el primer caracter (índice 0) y detenernos en el tercer caracter (índice 3). El resultado será la subcadena "Hol".

También podemos utilizar `subs` para extraer una subcadena en función de la posición final, utilizando un índice negativo. Por ejemplo, si queremos obtener los últimos tres caracteres de una cadena, podemos hacer lo siguiente:

```
Clojure (subs "Hola Mundo" -3)
"ndo"
```

En este caso, `subs` tomará la subcadena desde el tercer caracter a partir del final hasta el final de la cadena.

## Profundizando

En la función `subs` también podemos utilizar una sintaxis más específica para especificar el índice final, utilizando corchetes y un punto. Por ejemplo, si queremos obtener los caracteres de la posición 2 hasta la posición 4, podemos hacerlo así:

```
Clojure (subs "Hola Mundo" [2 . 5])
"la "
```

Esta sintaxis nos permite especificar un rango de índices que incluye el primer pero no el último caracter, por lo que en este ejemplo estamos obteniendo "la " como resultado.

También podemos utilizar expresiones regulares en la función `subs` para extraer subcadenas basadas en patrones específicos. Esto puede ser muy útil en aplicaciones de procesamiento de lenguaje natural. Aquí hay un ejemplo de cómo hacerlo:

```
Clojure (subs "Clojure es un lenguaje de programación funcional" #"[a-z]{3,5}")
"oju"
```

En este caso, estamos utilizando una expresión regular para encontrar sólo las subcadenas compuestas por entre 3 y 5 caracteres alfabéticos minúsculos. Como resultado, obtenemos "oju" de la palabra "Clojure".

## Ver También

- Documentación oficial de la función `subs`: https://clojuredocs.org/clojure.core/subs
- Tutorial sobre la manipulación de cadenas en Clojure: https://www.tutorialspoint.com/clojure/clojure_strings.htm
- Ejemplos de aplicaciones de procesamiento de lenguaje natural en Clojure: https://github.com/search?q=language%3Aclojure+natural+language+processing&type=Repositories