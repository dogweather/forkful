---
title:                "Uniendo cadenas de texto"
html_title:           "Go: Uniendo cadenas de texto"
simple_title:         "Uniendo cadenas de texto"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Concatenar cadenas (o strings) es un proceso común en programación que consiste en unir dos o más cadenas de texto para formar una nueva cadena. Los programadores lo hacen para crear mensajes más largos y complejos, combinar información de diferentes fuentes, o simplemente para mejorar la legibilidad del código.

## ¡Cómo hacerlo!

Para concatenar cadenas en Go, se utiliza el operador `+` entre las cadenas que se quieren unir. Por ejemplo:

```
Go
str1 := "Hola"
str2 := "mundo"
concat := str1 + " " + str2 // el resultado es "Hola mundo"
```

También es posible utilizar la función `fmt.Sprintf()` para concatenar cadenas mediante el uso de formato de impresión. Por ejemplo:

```
Go
str1 := "Hola"
str2 := "mundo"
concat := fmt.Sprintf("%s %s", str1, str2) // el resultado es "Hola mundo"
```

## Profundizando

El proceso de concatenar cadenas no es exclusivo de Go, ya que también se utiliza en otros lenguajes de programación. Sin embargo, en Go se pueden concatenar cadenas de manera eficiente, ya que el lenguaje está optimizado para manipular cadenas de forma eficiente.

Otra alternativa para concatenar cadenas en Go es utilizando `strings.Join()`, que permite unir múltiples cadenas con un separador específico. Por ejemplo:

```
Go
str1 := "Hello"
str2 := "world!"
concat := strings.Join([]string{str1, str2}, " ") // el resultado es "Hello world!"
```

A nivel de implementación, Go utiliza una estructura interna llamada `stringStruct` para representar cadenas, que incluye un puntero al primer elemento de la cadena y una longitud. Esto permite un acceso rápido a los caracteres individuales de una cadena.

## Ver también

Para más información sobre cómo trabajar con cadenas en Go, te recomendamos revisar la documentación oficial de strings en la [página de paquetes de la documentación de Go](https://golang.org/pkg/strings/) y el artículo [Working with strings in Go](https://blog.golang.org/strings) del blog oficial de Go.