---
title:    "Elm: Extrayendo subcadenas"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

##Por qué

Extraer subcadenas es una habilidad esencial para manipular y procesar cadenas de texto en Elm. Permite a los programadores seleccionar partes específicas de una cadena y utilizarlas para realizar otras operaciones.

##Cómo hacerlo

Para extraer una subcadena en Elm, se puede utilizar la función `String.slice` que toma dos argumentos: el índice de inicio y el índice de fin de la subcadena. Por ejemplo:

```Elm
String.slice 0 3 "Hola Mundo" -- devuelve "Hol"
```

También se puede utilizar la función `String.left` o `String.right` para extraer una cantidad específica de caracteres desde el inicio o el final de una cadena. Por ejemplo:

```Elm
String.left 3 "Hola Mundo" -- devuelve "Hol"
String.right 3 "Hola Mundo" -- devuelve "ndo"
```

Otra forma de extraer una subcadena es utilizando el operador `++` para concatenar una cadena vacía con la subcadena deseada. Por ejemplo:

```Elm
"Hola Mundo" ++ "" -- devuelve "Hola Mundo"
"Hola Mundo" ++ "do" -- devuelve "Hola Mundo"
```

##Profundizando

Además de las funciones mencionadas anteriormente, Elm también ofrece otras opciones para extraer subcadenas. Por ejemplo, se puede utilizar la función `String.take` para tomar una cantidad específica de caracteres desde el inicio de una cadena:

```Elm
String.take 5 "Hola Mundo" -- devuelve "Hola "
```

Otra opción es utilizar la función `String.drop` para eliminar una cantidad específica de caracteres desde el inicio de una cadena:

```Elm
String.drop 5 "Hola Mundo" -- devuelve " Mundo"
```

También se puede utilizar la función `String.split` para dividir una cadena en una lista de subcadenas basadas en un separador. Por ejemplo:

```Elm
String.split "," "Manzana,Plátano,Naranja" -- devuelve ["Manzana", "Plátano", "Naranja"]
```

##Ver también

- [Documentación oficial de Elm sobre cadenas de texto](https://elm-lang.org/docs/strings)
- [Guía de programación de Elm](https://www.elm-tutorial.org/es/)