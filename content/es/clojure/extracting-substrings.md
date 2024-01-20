---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Extraer subcadenas significa seleccionar una parte de un texto en una cadena. Los programadores lo hacen para analizar y procesar información específica en una cadena más grande.

## Cómo hacerlo:

En Clojure, extraer una subcadena es pan comido. Usamos la función `subs`.

```Clojure
(defn extraer-subcadena [cadena inicio fin]
  (subs cadena inicio fin))
```

Vamos a probarlo:

```Clojure
(extraer-subcadena "Hola Mundo!" 0 4)
```

El resultado será:

```Clojure
"Hola"
```

## Inmersión Profunda 

La función `subs` se ha usado en Lisp y sus dialectos, incluido Clojure, desde la antigüedad. Es simple y se usa comúnmente para dividir cadenas en partes más pequeñas.

Para extraer una subcadena en Clojure, puedes usar alternativamente las funciones incorporadas en java interop. Por ejemplo: `.substring`

```Clojure
(.substring "Clojure Programming" 0 7)
```

La implementación detrás de escena de `subs` es bastante eficiente. No crea una nueva cadena. En cambio, reutiliza el arreglo de caracteres de la cadena original.

## Consultar También

Para más detalles, consulta:
1. [Documentación oficial de Clojure sobre subs](https://clojuredocs.org/clojure.core/subs)
2. [Guía de programación de Clojure](https://www.braveclojure.com/clojure-for-the-brave-and-true/)
3. [Usando Java interop en Clojure](https://clojure.org/reference/java_interop)