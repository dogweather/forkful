---
title:                "Clojure: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas de texto es una habilidad esencial para trabajar con cadenas de caracteres en Clojure. Ya sea para manipular datos o para realizar operaciones de búsqueda, conocer cómo extraer subcadenas puede ser muy útil en la programación.

## Cómo hacerlo

Para extraer una subcadena de una cadena de texto en Clojure, puedes utilizar la función `subs`. Por ejemplo:

```Clojure
(def texto "Hola mundo")
(subs texto 1 4)
```

Este código devolverá la subcadena "ola" al especificar un índice de inicio de 1 (iniciando desde el segundo carácter) y un índice de fin de 4 (no incluyendo el cuarto carácter).

También puedes utilizar la función `substring` para lograr el mismo resultado:

```Clojure
(substring texto 1 4)
```

Sin embargo, `substring` es más eficiente que `subs` ya que no realiza ninguna copia de la cadena original, sino que comparte la misma cadena en memoria.

Si deseas extraer una subcadena desde un índice específico hasta el final de la cadena, puedes omitir el segundo parámetro en ambas funciones:

```Clojure
(subs texto 5) ; devolverá "mundo"
(substring texto 5) ; también devolverá "mundo"
```

## Profundizando

Cuando se trata de extraer subcadenas, es importante tener en cuenta que los índices en Clojure comienzan desde 0. Esto puede ser confuso para aquellos que están acostumbrados a otros lenguajes de programación en los que los índices comienzan desde 1.

Además, si el índice de fin es mayor que la longitud de la cadena, se devolverá una subcadena que va desde el índice de inicio hasta el final de la cadena. Por ejemplo:

```Clojure
(subs texto 0 500) ; devolverá la cadena completa "Hola mundo"
```

Por último, también puedes utilizar números negativos como índices en `substring` para contar desde el final de la cadena. Un índice de -1 indicaría el último carácter de la cadena.

## Ver también

- [Documentación de Clojure: Función subs](https://clojuredocs.org/clojure.core/subs)
- [Documentación de Clojure: Función substring](https://clojuredocs.org/clojure.core/substring)
- [Tutorial de Clojure: Manipulación de cadenas](https://clojure.org/guides/strings)