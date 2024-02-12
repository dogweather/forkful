---
title:                "Eliminando comillas de una cadena"
date:                  2024-01-26T03:38:35.026792-07:00
model:                 gpt-4-0125-preview
simple_title:         "Eliminando comillas de una cadena"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
Eliminar las comillas de una cadena significa deshacerse de esos molestos caracteres de comillas dobles o simples que envuelven tu texto. Los programadores hacen esto para limpiar datos, asegurar uniformidad o preparar cadenas para ser procesadas donde las comillas son indeseadas o pueden causar errores.

## Cómo hacerlo:
En Clojure, las cadenas son inmutables, así que cuando hablamos de "eliminar comillas", realmente estamos hablando de crear una nueva cadena sin comillas. Aquí está lo esencial usando `clojure.string/replace`:

```clojure
(require '[clojure.string :as str])

; Vamos a deshacernos de esas comillas dobles
(defn remove-double-quotes [s]
  (str/replace s #"\"" ""))

; Y expulsemos las comillas simples
(defn remove-single-quotes [s]
  (str/replace s #"\'" ""))

; Ejemplo de uso:
(remove-double-quotes "\"¡Hola, Mundo!\"") ; => "¡Hola, Mundo!"
(remove-single-quotes "'¡Hola, Mundo!'")   ; => "¡Hola, Mundo!"
```
¿Quieres manejar tanto las comillas simples como las dobles de un solo golpe? Echa un vistazo a esto:

```clojure
(defn remove-quotes [s]
  (str/replace s #"[\"\']" ""))

; Ejemplo de uso:
(remove-quotes "\"Hola, 'Clojure' Mundo!\"") ; => "Hola, Clojure Mundo!"
```

## Análisis profundo
En los viejos tiempos cuando los datos eran más desordenados que la habitación de un niño, las comillas en las cadenas eran la norma para denotar texto. Pero a medida que la informática evolucionó, las comillas se convirtieron en algo más que simples delimitadores de texto; asumieron roles sintácticos en los lenguajes de programación.

Clojure, con su herencia de Lisp, no usa las comillas de la misma manera que algunos otros lenguajes podrían hacerlo. Se utilizan para denotar cadenas, por supuesto, pero también tienen un papel especial en la creación de literales. Sin embargo, eliminar las comillas de las cadenas sigue siendo una tarea atemporal.

¿Por qué no simplemente cortar los extremos de una cadena? Bueno, eso supondría que tus comillas siempre están abrazando el inicio y el final de tu cadena como un par de abuelos excesivamente afectuosos. Los datos del mundo real son más desordenados. Entra en juego regex (expresiones regulares), que te permite apuntar a esas comillas sin importar dónde estén escondidas.

¿Alternativas? Claro, puedes ponerte elegante con `subs`, `trim`, `triml`, `trimr`, o incluso transductores si quieres presumir. Pero `replace` con regex es como llevar un sable de luz a una pelea de cuchillos: corta directo al grano.

## Ver También
Si tu cerebro tiene ganas de más bondades de manipulación de cadenas en Clojure, estos enlaces podrían ayudar:

- ClojureDocs sobre `clojure.string/replace`: https://clojuredocs.org/clojure.string/replace
- Expresiones regulares en Clojure: https://clojure.org/guides/learn/syntax#_regex
- Interoperabilidad con Java para el manejo de cadenas (después de todo, Clojure se ejecuta en la JVM): https://clojure.org/reference/java_interop#_working_with_strings

No te detengas solo en eliminar comillas. Hay todo un mundo de magia con cadenas allá afuera en Clojure-land esperando ser descubierto.
