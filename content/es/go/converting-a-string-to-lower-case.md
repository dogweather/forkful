---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Go: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías convertir una cadena de texto a minúsculas en Go?

Convertir una cadena de texto a minúsculas es una operación común en muchos programas, especialmente en aquellos que involucran el manejo de datos sensibles a mayúsculas y minúsculas, como contraseñas y nombres de usuario. En Go, esto se puede lograr de manera rápida y eficiente utilizando las funciones y métodos incorporados en el lenguaje.

## Cómo hacerlo en Go

```Go
cadena := "Hola, MUNDO!"

cadenaMin := strings.ToLower(cadena)
// resultado: hola, mundo!
```

En este ejemplo, utilizamos la función `ToLower()` del paquete `strings` para convertir una cadena de texto a minúsculas. También podemos usar el método `ToLower()` de la estructura `strings.Builder` para realizar la misma operación en un tipo de datos mutable.

```Go
builder := strings.Builder{}
builder.WriteString("HELLO, WORLD!")

builderMin := builder.ToLower()
// resultado: hello, world!
```

Además, si queremos convertir toda una cadena de texto a mayúsculas o a minúsculas, podemos utilizar las funciones `strings.ToUpper()` y `strings.ToLower()`, respectivamente.

## Un vistazo más profundo

En Go, las cadenas de texto se almacenan como un conjunto de bytes en memoria, donde cada byte representa un carácter. En el conjunto *utf-8* de caracteres, las letras mayúsculas y minúsculas tienen un valor numérico diferente, por lo tanto, para convertir una cadena de texto a minúsculas, simplemente necesitamos cambiar los valores numéricos correspondientes de cada caracter. La función `ToLower()` y el método `ToLower()` se encargan de realizar esta operación de manera eficiente.

## Ver también

- [Documentación oficial de Go sobre el paquete `strings`](https://golang.org/pkg/strings/)
- [Artículo sobre cadenas de texto en el blog de Go](https://blog.golang.org/strings)