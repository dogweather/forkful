---
title:                "Uniendo cadenas"
html_title:           "Go: Uniendo cadenas"
simple_title:         "Uniendo cadenas"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué
Concatenar strings, o unir cadenas, es una operación común en programación que nos permite combinar varias cadenas de texto en una sola. Esto es útil cuando queremos construir mensajes dinámicos o manipular datos de una forma específica.

## Cómo hacerlo
En Go, podemos concatenar strings utilizando el operador `+` o la función `strings.Join()`. Veamos un ejemplo de cada uno:

```Go
nombre := "Juan"
apellido := "Pérez"

// utilizamos el operador + para concatenar
cadena := nombre + " " + apellido
fmt.Println(cadena)

// utilizamos strings.Join()
cadena2 := strings.Join([]string{nombre, apellido}, " ")
fmt.Println(cadena2)
```

El resultado en ambos casos será `Juan Pérez`.

## Profundizando
Es importante tener en cuenta que al utilizar el operador `+` en Go, se crea una nueva cadena en memoria cada vez que se concatena algo. Por lo tanto, si esto se hace repetidamente sobre grandes cantidades de datos, puede ser ineficiente. En estos casos, es mejor utilizar la función `strings.Join()` ya que sólo se crea una nueva cadena al finalizar.

Además, es importante mencionar que Go representa las cadenas como un arreglo de bytes. Esto puede afectar la concatenación si se utilizan caracteres multibyte, como por ejemplo, aquellos con acentos o caracteres especiales. En estos casos, puede ser necesario utilizar la función `strings.Builder` para unir cadenas de forma eficiente.

## Ver también
- [Documentación oficial de Go sobre el paquete `strings`](https://golang.org/pkg/strings/)
- [Guía interactiva de Go en línea](https://go-tour-es.appspot.com/basics/1) para aprender más sobre el lenguaje.