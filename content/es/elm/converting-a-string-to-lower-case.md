---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Elm: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Convertir una cadena de texto a minúsculas es un procedimiento común en la programación que implica cambiar todas las letras mayúsculas en una cadena a sus equivalentes en minúsculas. Esto es útil para estandarizar el formato de texto y para hacer comparaciones de texto más precisas. Los programadores a menudo hacen esto al manipular datos de entrada o al comparar entradas de usuario.

## ¿Cómo se hace?

```Elm
import String

String.toLower "HOLA MUNDO" -- outputs "hola mundo"
String.toLower "Elm Programming" -- outputs "elm programming"
```

## Profundizando

Este proceso de convertir string a minúsculas ha sido utilizado desde los primeros días de la programación, cuando se hacían comparaciones de texto en los lenguajes de bajo nivel. Sin embargo, en Elm, esto se logra a través de la biblioteca estándar `String` en lugar de tener que codificarlo manualmente. Alternativamente, se pueden utilizar diferentes métodos, como `String.foldl` o `String.map`, para lograr el mismo resultado. La implementación de `String.toLower` en Elm usa la tabla Unicode para mapear las letras mayúsculas a sus correspondientes en minúsculas.

## Ver también

Consulte la documentación de la biblioteca `String` en la página oficial de Elm para más información sobre métodos de manipulación de cadenas en Elm. Además, también puede explorar otras formas de manipulación de textos en la biblioteca estándar de Elm como `String.replace` o `String.split`.