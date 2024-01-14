---
title:                "Clojure: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué usar la concatenación de cadenas?

La concatenación de cadenas es una técnica esencial en la programación, especialmente en Clojure. Permite combinar varias cadenas juntas para crear una cadena más larga y completa. Esto puede ser últil en la creación de mensajes, errores y en el manejo de datos.

## Cómo hacerlo:

La concatenación de cadenas en Clojure utiliza la función `str` que toma cualquier cantidad de argumentos y los combina en una sola cadena. Aquí hay un ejemplo:

```Clojure
(str "¡Hola!" " Mi nombre es " "Juan.")
```

El resultado sería "¡Hola! Mi nombre es Juan.". Como puedes ver, los espacios también se concatenan entre cada cadena.

También puedes utilizar la función `format` para crear una cadena con formato específico. Algo así:

```Clojure
(format "Mi nombre es %s y mi edad es %d años." "Juan" 27)
```

Aquí, el símbolo `%s` se reemplaza por la cadena "Juan" y el símbolo `%d` por el número 27.

## Profundizando:

Además de las funciones `str` y `format`, Clojure también cuenta con la función `join` que permite concatenar una secuencia de cadenas, uniendo cada una con un separador específico. Por ejemplo:

```Clojure
(join "-" ["rojo" "verde" "azul"])
```

El resultado sería "rojo-verde-azul", ya que el separador elegido fue el guión "-".

También puedes combinar estas funciones para obtener resultados más complejos. Por ejemplo, puedes utilizar `join` dentro de `str` para crear un mensaje personalizado:

```Clojure
(str "¡Hola! Soy " (join " " ["Juan" "el" "programador"]))
```

El resultado sería "¡Hola! Soy Juan el programador.". Como puedes ver, la concatenación de cadenas ofrece muchas posibilidades y es una técnica importante para cualquier programador en Clojure.

## Ver también:

- [Documentación oficial de concatenación de cadenas en Clojure](https://clojuredocs.org/clojure.core/str)
- [Tutorial de concatenación de cadenas en Clojure](https://www.tutorialspoint.com/clojure/clojure_string_concatenation.htm)
- [Otros ejemplos de concatenación en Clojure](https://4clojure.com/problem/108)