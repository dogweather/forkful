---
title:                "Clojure: Búsqueda y reemplazo de texto"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

##¿Por qué?

Realizar búsquedas y reemplazos de texto en tus programas Clover puede ahorrarte tiempo y esfuerzo en la corrección de errores y actualización de código.

##Cómo hacerlo

Los comandos `replace` y `replace-all` se utilizan para realizar búsquedas y reemplazos en una cadena de texto especificada. Por ejemplo, si queremos reemplazar todas las letras "a" con la letra "e" en la palabra "Casa", escribiríamos lo siguiente:

```Clojure
(replace "Casa" \a \e)
```

Este comando producirá la salida "Cese". Si queremos reemplazar todas las letras "a" con la letra "e" en todo un texto, podemos utilizar el comando `replace-all` de la siguiente manera:

```Clojure
(replace-all "Mi casa es azul" \a \e)
```

La salida de este comando sería "Mi cese es ezul". Además de letras, también se pueden utilizar símbolos y cadenas de texto completas en reemplazos.

##Profundizando

Para realizar búsquedas y reemplazos más complejos, se pueden utilizar expresiones regulares en lugar de símbolos. Por ejemplo, si queremos reemplazar todos los números en un texto con una cadena vacía, podemos usar la expresión regular `#"[0-9]+"` junto con el comando `replace-all`:

```Clojure
(replace-all "La fecha es 02/10/20" #"[0-9]+" "")
```

La salida de este comando sería "La fecha es //". También se pueden utilizar grupos de captura en las expresiones regulares para realizar reemplazos más específicos.

##Ver también

- [Documentación oficial de comandos de búsqueda y reemplazo en Clojure](https://clojure.org/guides/text_processing)
- [Tutorial de programación en Clojure en español](https://www.tutorialspoint.com/clojure/index.htm)
- [Repositorio de código de ejemplo en Clojure](https://github.com/clojure/clojurescript)