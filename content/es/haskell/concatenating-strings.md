---
title:                "Concatenando cadenas de texto"
html_title:           "Arduino: Concatenando cadenas de texto"
simple_title:         "Concatenando cadenas de texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué es y Por Qué?

La concatenación de cadenas en programación es la operación de juntar secuencias de caracteres. Facilita la manipulación de datos y mejora la interacción con los usuarios al permitir la creación de mensajes dinámicos.

## Cómo se hace:

En Haskell, utilizamos el operador `++` para concatenar cadenas. Veamos un ejemplo:

```Haskell
greeting = "Hola, " ++ "amigo!"
print(greeting)
```

El output será:

```
Hola, amigo!
```

## Profundizando

La concatenación de cadenas puede parecer simple, pero tiene una historia rica. Originó en los primeros lenguajes de programación y ha evolucionado en su funcionalidad y eficiencia con el tiempo.

En cuanto a las alternativas, puedes usar la función `concat` en lugar del operador `++`:

```Haskell
greeting = concat ["Hola, ", "amigo!"]
print(greeting)
```

Obtendrás el mismo resultado: `Hola, amigo!`.

Al usar `++` o `concat`, Haskell crea internamente una nueva cadena, lo que puede tener implicaciones de rendimiento si estás trabajando con cadenas muy largas. Para concatenar cadenas de manera más eficiente, puedes considerar el uso de `Data.Text` o `Data.ByteString`.

## Ver También

Puedes leer más sobre la concatenación de cadenas en Haskell en los siguientes recursos:

1. Learn You a Haskell for Great Good! @ http://learnyouahaskell.com/starting-out#strings
2. Real World Haskell @ http://book.realworldhaskell.org/read/efficient-file-processing-regular-expressions-and-file-name-matching.html#idp10774832
3. Haskell Wiki @ https://wiki.haskell.org/Performance/Strings