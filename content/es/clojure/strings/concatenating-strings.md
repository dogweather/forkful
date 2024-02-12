---
title:                "Concatenación de cadenas de texto"
aliases: - /es/clojure/concatenating-strings.md
date:                  2024-01-20T17:34:27.930309-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenación de cadenas de texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Concatenar cadenas es simplemente unir dos o más textos en uno. Los programadores lo hacen para construir mensajes, combinar información y construir URLs entre otros usos prácticos.

## Cómo hacerlo:
```Clojure
; Usar 'str' para concatenar cadenas:
(str "Hola, " "¿cómo " "estás?")
; Resultado: "Hola, ¿cómo estás?"

; Concatenar usando 'str' con números y otros valores:
(str "El resultado es: " 42)
; Resultado: "El resultado es: 42"

; Usar 'join' para concatenar con separador:
(clojure.string/join ", " ["manzanas" "naranjas" "bananas"])
; Resultado: "manzanas, naranjas, bananas"
```

## Análisis Profundo:
Históricamente, concatenar cadenas ha sido vital en programación desde que necesitamos interacciones dinámicas con el usuario. En Clojure, `str` es la función más directa y eficiente. Si bien hay otras maneras, como usar `StringBuilder` en Java interop, `str` es más limpio en código Clojure. `join` de `clojure.string` es excelente cuando se trata de listas y necesitas un separador específico.

Alternativas como `format` habilitan interpolación de strings, una opción poderosa cuando necesitas un template más complejo para tus strings. La eficiencia de estas operaciones es usualmente alta, pero cuando se trata de concatenar grandes cantidades de datos, es mejor considerar estructuras de datos más eficientes, como la manipulación de secuencias antes de la conversión final a cadena.

## Ver También:
- ClojureDocs para ejemplos de `str`: https://clojuredocs.org/clojure.core/str
- Guía de estilo Clojure recomendando uso de `str`: https://guide.clojure.style/#prefer-str-over-usage
- Documentación de Clojure sobre cadenas de caracteres y funciones de utilidad: https://clojure.org/guides/learn/strings
