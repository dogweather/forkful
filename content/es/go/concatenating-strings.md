---
title:                "Go: Concatenando cadenas"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué concatenar cadenas en Go?

Concatenar cadenas en Go es una técnica común en programación que permite unir dos o más cadenas de texto en una sola. Esto es útil en situaciones donde se desea crear una salida de texto más compleja o agregar información dinámica a una cadena existente.

## Cómo hacerlo:

En Go, el operador `+` se utiliza para concatenar cadenas. Por ejemplo, si queremos unir las cadenas "¡Hola" y "mundo!", podemos utilizar el siguiente código:

```Go
saludo := "¡Hola"
mundo := "mundo!"
fmt.Println(saludo + " " + mundo)
```

Esto imprimirá la cadena "¡Hola mundo!" en la pantalla. También se puede utilizar la función `strings.Join()` para concatenar más de dos cadenas a la vez, utilizando un delimitador opcional.

```Go
paises := []string{"España", "México", "Argentina"}
fmt.Println(strings.Join(paises, ", "))
```

Esto imprimirá "España, México, Argentina" en la pantalla. Se pueden concatenar cadenas de diferentes tipos, como cadenas y variables enteras o flotantes.

En Go, las cadenas de texto se pueden manipular utilizando la librería `strings`, que ofrece funciones útiles para dividir, reemplazar y buscar texto dentro de una cadena.

## Una mirada más profunda:

El proceso de concatenar cadenas en Go se basa en la creación de una nueva cadena a partir de dos o más cadenas existentes. Esto significa que se asigna un nuevo bloque de memoria para contener la cadena concatenada.

Es importante tener en cuenta que, debido a que las cadenas en Go son inmutables, cada vez que se concatena una cadena, se crea una nueva versión en lugar de modificar la original. Esto puede tener un impacto en el rendimiento si se concatena un gran número de cadenas en un ciclo o bucle.

Además, como se mencionó anteriormente, la librería `strings` ofrece funciones para manipular cadenas de texto de manera más eficiente y con menos impacto en el rendimiento.

## Ver también:

- Documentación oficial de Go sobre cadenas: https://golang.org/pkg/strings/
- Ejemplos de concatenación de cadenas en Go: https://gobyexample.com/string-concatenation
- Tutorial de programación básica de Go: https://www.freecodecamp.org/news/learn-golang/