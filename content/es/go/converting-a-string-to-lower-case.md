---
title:                "Go: Convirtiendo una cadena a minúsculas"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, a menudo necesitamos manipular cadenas de texto y una de las tareas más comunes es convertir una cadena a minúsculas. Esto puede ser útil para comparar cadenas sin tener en cuenta las diferencias de mayúsculas y minúsculas, o para un formato específico de salida. ¡Aprender cómo hacerlo en Go puede ahorrar mucho tiempo y esfuerzo!

## Cómo hacerlo

```Go
func toLowerCase(s string) string {
    var result string
    for i := 0; i < len(s); i++ {
        if s[i] >= 'A' && s[i] <= 'Z' {
            result += string(s[i] + 32)
        } else {
            result += string(s[i])
        }
    }
    return result
}

fmt.Println(toLowerCase("HOLA MUNDO"))
```

Resultado: hola mundo

En este ejemplo, estamos utilizando un bucle para recorrer cada caracter de la cadena y utilizando la tabla ASCII para convertir las letras mayúsculas a minúsculas. También podría utilizarse la función `strings.ToLower()` para lograr el mismo resultado.

## Profundizando

Existen varias formas de convertir una cadena a minúsculas en Go, incluyendo el uso de la función `strings.ToLower()`, la librería `unicode` y los operadores lógicos bit a bit. Sin embargo, la forma más eficiente es utilizando la tabla ASCII y operaciones aritméticas, como se muestra en el ejemplo anterior. También es importante tener en cuenta que la conversión a minúsculas puede variar según el conjunto de caracteres utilizado.

## Ver también

- [Documentación de Go sobre cadenas](https://golang.org/pkg/strings/)
- [Convertir cadenas a mayúsculas y minúsculas en Go](https://www.geeksforgeeks.org/converting-string-into-lowercase-uppercase-using-ascii-values-c/)
- [Tabla ASCII](https://es.wikipedia.org/wiki/ASCII)