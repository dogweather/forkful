---
title:                "Concatenando cadenas de texto"
html_title:           "Arduino: Concatenando cadenas de texto"
simple_title:         "Concatenando cadenas de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La concatenación de strings es el proceso de juntar dos o más strings para formar uno más grande. Los programadores la utilizan para manipular información y presentarla en formatos específicos.

## ¿Cómo se hace?

En Gleam, puedes concatenar strings utilizando el operador `++`. Aquí te dejamos un ejemplo:

```Gleam
fn main() {
  let hola = "Hola"
  let mundo = "mundo"
  let mensaje = hola ++ ", " ++ mundo
  mensaje
}
```
Esto imprimiría:

```Gleam
"Hola, mundo"
```

## Un vistazo en detalle

1) En el contexto histórico, desde los primeros lenguajes de programación, la concatenación de strings ha sido una función esencial. La implementación específica y la sintaxis pueden variar, pero el concepto es universal.

2) Como alternativa en Gleam, también puedes usar la función `string.concat`. Esto puede ser más legible cuando se concatenan varios strings.

```Gleam
fn main() {
  let mensaje = string.concat(["Hola", ", ", "mundo"])
  mensaje
}
```

Esto también imprimiría:

```Gleam
"Hola, mundo"
```

3) Detalles de implementación: En Gleam, la concatenación de strings con `++` y `string.concat` es eficiente, ya que ambos métodos se traducen directamente a concatenación de strings en Erlang, que es una operación eficiente debido a la inmutabilidad de strings en la plataforma BEAM.

## Ver también

- Documentación oficial de Gleam: [Operaciones de strings](https://gleam.run/book/tour/strings.html)
- Tutorial de Erlang: [Concatenación de strings](http://learnyousomeerlang.com/starting-out-for-real#strings-and-binaries)