---
title:                "Capitalizando una cadena"
html_title:           "Elm: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

¡Hola amigos programadores! En este artículo, vamos a hablar sobre cómo capitalizar una cadena de texto en Elm. ¿Qué es esto y por qué es importante? ¡Sigue leyendo para descubrirlo!
## Qué y Por qué?
La capitalización de una cadena de texto se refiere a escribir la primera letra de cada palabra con mayúscula, dejando las demás letras en minúscula. ¿Por qué es importante? Porque ayuda a que nuestro texto se vea limpio y ordenado, especialmente en aplicaciones que involucran texto visible para el usuario. 
## Cómo hacerlo:
Para capitalizar una cadena de texto en Elm, podemos utilizar la función `String.capitalize` que toma una cadena de texto y devuelve una nueva cadena con la primera letra de cada palabra capitalizada. Veamos un ejemplo:
```Elm
cadena = "hola amigos"
String.capitalize cadena
-- salida: "Hola Amigos"
```
Fácil, ¿verdad? También podemos utilizar la función `String.toUpper` que convierte toda la cadena en mayúsculas, y luego utilizar la función `String.unwords` para convertir la cadena en una lista de palabras y aplicar `String.capitalize` a cada una de ellas. Este enfoque nos permite capitalizar cada palabra en una cadena que contenga más de una palabra. Veamos otro ejemplo:
```Elm
cadena = "elm es genial"
String.unwords (List.map String.capitalize (String.words cadena))
-- salida: "Elm Es Genial"
```
## Profundizando más:
Para aquellos interesados en el contexto histórico, capitalizar una cadena de texto se remonta a los primeros lenguajes de programación y tiene sus raíces en la convención de nombrar variables y funciones utilizando la notación de camello (camelCase). Alternativamente, también tenemos la función `String.toTitle`, que capitaliza la primera letra de cada palabra en una cadena y convierte todas las demás letras en minúsculas. En cuanto a la implementación, estas funciones utilizan el Unicode del sistema para manejar caracteres de diferentes idiomas. 
## Ver también:
Si quieres aprender más sobre manipulación de cadenas de texto en Elm, puedes consultar la documentación oficial de la librería `String`, donde encontrarás más funciones útiles como `String.reverse` o `String.trim`. También puedes explorar la comunidad activa de Elm y aprender de otros programadores en foros como r/elm y elm-discuss@googlegroups.com. ¡Hasta la próxima!